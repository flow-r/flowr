

#' Run automated Pipelines
#'
#' @description
#' Run complete pipelines, by wrapping several steps into one 
#' convinient function.
#' 
#' NOTE: please use flowr version 0.9.8.9010 or higher.
#' 
#' In summary, this function performs the following steps:
#' 
#' \itemize{
#' \item the argument \code{x} defines the name of the pipeline. 
#' Say, for example \code{sleep_pipe}.
#'  \item \link{fetch_pipes}: finds the pipeline definition 
#'  (\code{sleep_pipe.R}, \code{sleep_pipe.def} and
#'  \code{sleep_pipe.conf} files)
#'  \item \code{sleep_pipe(\dots)}: Create all the required commands 
#'  (\code{flowmat})
#'   \item \link{to_flow}: Use \code{flowmat} and 
#'   \code{sleep_pipe.def} to create a flow object.
#'   \item \link{submit_flow}: Submit the flow to the cluster.
#' }
#'
#' @param x name of the pipeline to run. This is a function called to create a flow_mat.
#' @param def flow definition
#' @param flow_run_path passed onto to_flow. Default it picked up from flowr.conf. Typically this is ~/flowr/runs
#' @param wd an alias to flow_run_path
#' @param rerun_wd if you need to run, supply the previous working dir
#' @param start_from the step to start a rerun from. Intitutively, this is ignored in a fresh run and only used in re-running a pipeline.
#' @param conf a tab-delimited configuration file with path to tools and default parameters. See \link{fetch_pipes}.
#' @param platform what platform to use, overrides flowdef
#' @param execute TRUE/FALSE
#' @param ... passed onto the pipeline function as specified in x
#'
#'
#' @export
#' @importFrom params load_opts read_sheet write_sheet
#'
#' @aliases run_flow
#' 
#' @examples \dontrun{
#' 
#' ## Run a short pipeline (dry run)
#' run("sleep_pipe")
#' 
#' ## Run a short pipeline on the local machine
#' run("sleep_pipe", platform = "local", execute = TRUE)
#' 
#' ## Run a short pipeline on the a torque cluster (qsub)
#' run("sleep_pipe", platform = "torque", execute = TRUE)
#' 
#' ## Run a short pipeline on the a MOAB cluster (msub)
#' run("sleep_pipe", platform = "moab", execute = TRUE)
#' 
#' ## Run a short pipeline on the a IBM (LSF) cluster (bsub)
#' run("sleep_pipe", platform = "lsf", execute = TRUE)
#' 
#' ## Run a short pipeline on the a MOAB cluster (msub)
#' run("sleep_pipe", platform = "moab", execute = TRUE)
#' 
#' ## change parameters of the pipeline
#' ## All extra parameters are passed on to the function function.
#' run("sleep_pipe", platform = "lsf", execute = TRUE, x = 5)
#' 
#' }
run <- function(x,
                platform,
                def, conf, 
                wd = opts_flow$get("flow_run_path"),
                flow_run_path = wd,
                rerun_wd, start_from,
                execute = FALSE,  ...){
  
  #print(opts_flow$get("flow_run_path"))
  ## find a Rscript with name {{x}}.R
  
  message("\n> fetching pipeline... ")
  pip = fetch_pipes(x, last_only = TRUE)
  
  if(missing(x))
    stop("Please choose a pipeline to run, from the above list.")
  
  
  ## --- source the file and get the main function from it
  source(pip$pipe, TRUE); # may load addional conf
  # find function of the original name
  func = get(basename(x)) 
  
  
  message("\n> loading confs....")
  ## load default options for the pipeline
  confs = c(
    file.path(path.expand("~"), ".flowr.conf"),
    fetch_conf("flowr.conf"),
    # fetch_conf("ngsflows.conf"),
    pip$conf)
  confs = na.omit(confs)
  
  if(!missing(conf))
    confs = c(confs, conf)
  
  print(kable(as.data.frame(confs)))
  opts_flow$load(confs, verbose = FALSE, check = FALSE)

  message("\n> creating flowmat....")
  ## crate a flowmat
  args <- list(...)
  out = do.call(func, args)
  
  ## fetched from the latest conf file ONLY
  module_cmds = opts_flow$get("module_cmds")
  
  message("\n> stitching a flow object....")
  ## get a flowdef
  if(missing(def))
    def = as.flowdef(pip$def)
  
  if(missing(rerun_wd)){
    # create a flow object
    fobj = to_flow(x = out$flowmat,
                   def = def,
                   platform = platform,
                   flowname = basename(x),
                   module_cmds = module_cmds,
                   flow_run_path = flow_run_path)
    
    # submit the flow
    message("\n--> submitting ...")
    fobj = submit_flow(fobj, execute = execute)
    
  }else{
    
    fobj = rerun(x = rerun_wd, mat = out$flowmat, def = def, start_from = start_from, execute = execute)
    
  }
  
  invisible(fobj)
}


#' @rdname run
#' @export
run_pipe <- run


if(FALSE){
  
  debug(run_pipe)
  run_pipe("sleep_pipe", samplename = "samp2")
  
}

## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

.run <- function(x = "sleep", type = "example", platform, flowmat, def, execute = FALSE, ...){
  .Deprecated("run")
  library(flowr)
  message("\n\nPerforming initial setup....")
  setup()
  message("Running example on platform:\t\t\t", platform)
  if(is.character(x))
    if(x == "sleep")
      fobj <- .run_sleep(platform = platform, ...)
  
  ## x is the name of the function
  
  
  tmp <- submit_flow(fobj, execute = execute)
  return("Done !")
}






