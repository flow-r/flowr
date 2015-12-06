
if(FALSE){
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/ESCA/logs/esca-2015-03-09-00-05-36-tfhaScFP/"
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/BRCA/logs/brca-2015-02-17-12-42-32-MCscE2AW"
}


#' @title 
#' Re-run a pipeline in case of hardware or software failures.
#' @description 
#' 
#' \itemize{
#' \item \strong{hardware} no change required, simple rerun: \code{rerun(x=flow_wd)}
#' \item \strong{software} either a change to flowmat or flowdef has been made: \code{rerun(x=flow_wd, mat = new_flowmat, def = new_flowdef)}
#' }
#'
#' \strong{NOTE:}
#'
#' 
#' \emph{flow_wd}: flow working directory, same input as used for \link{status}
#'
#' @param x flow working directory
#' @param execute [logical] whether to execute or not
#' @param start_from (required) which job to start from, this is a job name.
#' @param select (optional) select a subset of jobs to rerun [character vector]
#' @param ignore (optional) ignore a subset of jobs to rerun [character vector]
#' @param mat (optional) flowmat fetched from previous submission if missing. For more information regarding the format refer to \link{to_flowmat}
#' @param def (optional) flowdef fetched from previous submission if missing.  For more information regarding the format refer to \link{to_flowdef}
#' @param kill (optional) logical indicating whether to kill the jobs from the previous execution of flow.
#' @inheritParams to_flow
#' @param samplename (optional) If flowmat contains multiple samples, provide the samplename, 
#' corresponding to the flow working directory provided.
#' @param ... passed onto to_flow
#'
#'
#' @details 
#' This function fetches details regarding the previous execution from the flow working directory (flow_wd). 
#' 
#' It reads the \link{flow} object from the flow_details.rds file, and extracts flowdef and flowmat from it 
#' using \link{to_flowmat} and \link{to_flowdef} functions.
#' 
#' \strong{Using new flowmat OR flowdef for re-run}:
#' 
#' Optionally, if either of flowmat or flowdef are supplied; supplied ones are used instead of those
#' extracted from previous submission.
#' 
#' This functions efficiently updates job details of the latest submission into the previous file; thus information
#' regarding previous job ids and their status is not lost.
#' 
#' @export
#' @examples \dontrun{
#' 
#' # 
#' rerun(wd = wd, fobj = fobj, execute = TRUE, kill = TRUE)
#' 
#' }
rerun <- function(x, ...) {
  
  if(opts_flow$get("verbose") > 1)
    message("rerun: input x is ", class(x))
  
  UseMethod("rerun")
}


#' @rdname rerun
#' @export
rerun.character <- function(x, ...){
  
  message("> input looks like a path, seeing if multiple paths match ...")
  wds = get_wds(x)
  
  tmp <- lapply(wds, function(wd){
    
    message("> reading flow_details.rds from: ", wd)
    wd = file_path_as_absolute(wd)
    fobj <- read_fobj(wd)
    
    if(is.character(fobj))
      stop("x does not seems to be a correct path to the flow submission, missing flow_details.rds")
    
    args = list(...)
    if(any(names(args) %in% c("flowmat", "flowdef", "flow_mat", "flow_def")))
      stop("some arguments not recognized\n",
           "Valid arguments for rerun are: mat and def, for flow matrix and flow definition respectively.")
    
    # updating this with the new path
    fobj@flow_run_path = dirname(wd)
    fobj@flow_path = wd
    
    rerun(fobj, ...)
  })
  
  invisible(tmp)
  
  
}


#' @rdname rerun
#' @importFrom utils capture.output
#' @importFrom params kable
#' @export
rerun.flow <- function(x, mat, def, 
                       start_from,
                       samplename,
                       execute = TRUE,
                       kill = TRUE, 
                       select,
                       ignore,
                       verbose = opts_flow$get("verbose"),
                       ...){
  fobj = x
  
  wd = fobj@flow_path
  
  assert_version(fobj, '0.9.7.3')
  assert_status(fobj, "submitted")
  
  ## converting missing to NA, so that passing to subsequent 
  ## steps is easier
  if(missing(start_from))
    start_from=NA
  if(missing(select))
    select=NA
  if(missing(ignore))
    ignore=NA
  
  if(is.na(start_from) & is.na(ignore[1]) & is.na(select[1])){
    stop("start_from, select, ignore: missing\n", 
         "Detection of failure point is currently not supported. ",
         "Please mention what steps need to be re-run.\n", 
         "Use start_from=<jobname>\n",
         "OR select=<jobnames> OR ignore=<jobnames>")
    #start_from = detect_redo()
  }
  
  if( !is.na(ignore[1]) & !is.na(select[1]) )
    stop("both ignore and select specified\n",
         "Either specify jobs to re-run using select OR ",
         "jobs to be ignored using re-run, not both")
  
  if(missing(def)){
    #stop("Please metion where to start from. Detection currently no supported")
    message("\n> extracting flow definition from previous run.")
    def = to_flowdef(fobj)
  }else{
    message("\nReading flow definition supplied.")
    def = as.flowdef(def) ## as jobids now !
  }
  
  if(missing(mat)){
    message("> Extracting flow mat (shell cmds) from previous run.")
    message("> Hope the reason for previous failure was fixed...")
    mat = to_flowmat(fobj)
  }else{
    mat = as.flowmat(mat)
    if(!missing(samplename)){
      message("> subsetting for sample: ", 
              samplename, " starting with rows ", nrow(mat))
      samp = samplename  
      mat = subset(mat, mat$samplename == samp)
      message("--> now we have: ", nrow(mat), " rows")
      if(nrow(mat) == 0)
        stop("--> no jobs left after subsetting ...")
    }
  }
  
  
  message("\n> subsetting... get steps to re-run:")
  mat = subset_fmat(fobj = fobj, mat = mat, start_from = start_from, select, ignore)
  def = subset_fdef(fobj = fobj, def = def, start_from = start_from, select, ignore)
  message(paste("--> ", def$jobname, collapse = "\n"), "\n")
  
  
  ## reset few things before we start the new flow
  ## kill the flow
  if(kill)
    try(kill(wd))
  
  ## remove trigger files
  det = to_flowdet(fobj)
  newdet = subset_fdet(fobj, det, start_from = start_from, select, ignore)
  if(execute) 
    newdet = file.remove(newdet$trigger)
  
  ## jobname, has ids as well.
  # flow_run_path: does not matter, since in the next step we would use flow_path
  fobj2 <- to_flow(x = mat, def = def, 
                   flowname = fobj@name, 
                   flow_run_path = fobj@flow_run_path, ...)
  
  #knitr::kable(rerun)
  fobj2@status = "rerun"
  fobj2 <- submit_flow(fobj2,
                       uuid = fobj@flow_path,
                       execute = execute,
                       dump = FALSE)
  
  ## -- need a function to read and update the old flow object with new job submission ids
  fobj = update.flow(fobj, child = fobj2)
  ## new flowdet of the new flow
  flowdet = to_flowdet(fobj)
  write_flow_details(wd, fobj, flow_det = flowdet)
  
  invisible(fobj)
}



update.flow <- function(x, child){
  
  child_jobs = jobnames(child)
  ## --- for each job in child update ids
  ## updat the whole job, and not just IDs
  for(j in child_jobs){
    #x@jobs[[j]]@id = child@jobs[[j]]@id
    x@jobs[[j]] = child@jobs[[j]]
  }
  return(x)
  
}

#' @importFrom params read_sheet
detect_redo <- function(fobj, wd){
  ## get detail file
  det_file = to_flowdet(wd)
  get_status(x = wd)
  ## subset those which need to be rerun
  flow_status = read_sheet(det_file)
  #plot_flow(x = fobj, pdf = TRUE)
  ## ingest everything in the fobj !
  #head(flow_status)
  mods = unique(as.character(flow_status$jobnm))
  fobj2 = fobj
  fobj2@status = "" ## reset flow status, will be submitted as a independent flow
  for(m in mods){
    fobj@jobs[[m]]@exit_code = subset(flow_status, flow_status$jobnm == m)$exit_code
    ## if we do not know the exit code, we redo
    redo = !fobj@jobs[[m]]@exit_code == 0;redo = ifelse(is.na(redo), TRUE, redo)
    ## need to subset CMDS,
    fobj2@jobs[[m]]@cmds = fobj2@jobs[[m]]@cmds[ redo ]
    fobj2@jobs[[m]]@dependency = list() ## dependent job ids
    fobj2@jobs[[m]]@id = vector(mode = "character")
    fobj2@jobs[[m]]@exit_code = vector(mode = "numeric")
  }
}

subset_mods <- function(fobj, start_from, select, ignore){
  
  mods = names(fobj@jobs)
  
  ## subset jobs using, start from, ignore and select
  if(!missing(start_from) & !all(is.na(start_from)) )
    mods = mods[which(mods == start_from):length(mods)]
  
  ##mods = mods[which(grepl(start_from, mods)):length(mods)]
  
  if(!missing(select) & !all(is.na(select)))
    mods = mods[mods %in% select]
  
  if(!missing(ignore) & !all(is.na(ignore)))
    mods = mods[!mods %in% ignore]
  
  return(mods)
}

# subset_fmat
#
# @param mat a part of flowmat
# @param fobj flow object
#' @inheritParams rerun
subset_fmat <- function(fobj, mat, start_from, select, ignore){
  
  mods = subset_mods(fobj, start_from, select, ignore)
  
  ## get mat
  mat = subset(mat, mat$jobname %in% mods)
  ## subset and get jobs which failed
  #rerun = cbind(module = mods, rerun = unlist(lapply(mods, function(m) length(fobj@jobs[[m]]@cmds))))
  return(mat)
}



# subset_fdef
# @param fobj flow object
# @param def flowdef
#' @inheritParams rerun
subset_fdef <- function(fobj, def, start_from, select, ignore){
  
  if(missing(def))
    stop("Please supply a flow def file")
  
  mods = subset_mods(fobj, start_from, select, ignore)
  
  ## get mat
  def = subset(def, def$jobname %in% mods)
  def$prev_jobs = ifelse(def$prev_jobs %in% mods, def$prev_jobs, "none")
  def$dep_type = ifelse(def$prev_jobs %in% mods, def$dep_type, "none")
  
  return(def)
}

# subset flow details file
# @param fobj flow object
# @param det flowdet
#' @inheritParams rerun
subset_fdet <- function(fobj, det, start_from, select, ignore){
  
  if(missing(det))
    stop("Please supply a flow det file")
  
  mods = subset_mods(fobj, start_from, select, ignore)
  
  ## get mat
  det = subset(det, det$jobnm %in% mods)
  return(det)
}





