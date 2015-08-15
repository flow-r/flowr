
if(FALSE){
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/ESCA/logs/esca-2015-03-09-00-05-36-tfhaScFP/"
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/BRCA/logs/brca-2015-02-17-12-42-32-MCscE2AW"
}

#' @title rerun
#' @description rerun
#'
#'
#' @param x Either path to flow folder or the \link{flow} object which has been 'returned' from \link{submit_flow}.
#' @param execute [logical] whether to execute or not
#' @param kill logical indicating whether to kill the jobs from old flow
#' @param start_from which job to start from
#' @param mat path to flow_mat. should fetch on the fly
#' @param def path to should fetch on the fly
#' @param ... not used
#'
#'
#' @details We need path to the flow folder (\code{wd}). The \link{flow} object needs to have upate 'base_path' slow with wd (the path to the flow folder). Also its important to know that we need details regarding the previous submission from flow_details.txt file. Which should typically be in \code{wd}
#' @export
#' @examples \dontrun{
#' rerun_flow(wd = wd, fobj = fobj, execute = TRUE, kill = TRUE)
#' }
#'  @export
rerun <- function(x, ...) {
	if(get_opts("verbose"))
		message("rerun: input x is ", class(x))
	UseMethod("rerun")
}


#' @rdname rerun
#' @export
rerun.character <- function(x, ...){
	message("x looks like a path, reading flow_details.rds")
	fobj <- read_fobj(x)

	if(is.character(fobj))
		stop("x does not seems to be a correct path to the flow submission, missing flow_details.rds")

	rerun(fobj, ...)

}


#' @rdname rerun
#' @importFrom utils capture.output
#' @export
rerun.flow <- function(x, mat, def, start_from,
											 execute = TRUE, kill = TRUE, ...){
	fobj = x
	wd = fobj@flow_path

	assert_version(fobj, '0.9.7.3')

	if(missing(start_from)){
		stop(error("no.start_from"))
		#start_from = detect_redo()
	}

	if(missing(def)){
		#stop("Please metion where to start from. Detection currently no supported")
		message("\nExtracting flow definition from previous run.")
		def = to_flowdef(fobj)
	}else{
		def = as.flowdef(def) ## as jobids now !
	}

	if(missing(mat)){
		message("Extracting flow mat (shell cmds) from previous run.")
		message("Hope the reason for previous failure was fixed...")
		mat = to_flowmat(fobj)
	}else{
		mat = as.flowmat(mat)
	}

	## kill the flow
	if(kill)
		capture.output(try(kill(wd)),
			file = file.path(wd, "kill_jobs.out"))

	message("\nSubsetting... get stuff to run starting, ", start_from, ":\n")
	mat = subset_fmat(fobj = fobj, mat = mat, start_from = start_from)
	def = subset_fdef(fobj = fobj, def = def, start_from = start_from)
	message(paste(def$jobname, collapse = "\n"))

	## jobname, has ids as well.
	fobj2 <- to_flow(x = mat, def = def, flowname = fobj@name, flow_run_path = fobj@flow_run_path)

  #knitr::kable(rerun)
	fobj2@status = "rerun"
	fobj2 <- submit_flow(fobj2,
		uuid = fobj@flow_path,
		execute = execute,
		dump = FALSE)

  ## -- need a function to read and update the old flow object with new job submission ids
  fobj = update.flow(fobj, child = fobj2)

  flowdet = to_flowdet(fobj)
  write_flow_details(wd, fobj, flow_det = flowdet)

  return(fobj)
}



update.flow <- function(x, child){

	child_jobs = jobnames(child)
	## --- for each job in child update ids
	for(j in child_jobs){
		x@jobs[[j]]@id = child@jobs[[j]]@id
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

#' subset_fmat
#'
#' @param fobj flow object
#' @param mat a part of flowdef
#' @param start_from, where to start from
subset_fmat <- function(fobj, mat, start_from){
	mods = names(fobj@jobs)
	mods = mods[which(grepl(start_from, mods)):length(mods)]
	## get mat
	mat = subset(mat, mat$jobname %in% mods)
	## subset and get jobs which failed
	#rerun = cbind(module = mods, rerun = unlist(lapply(mods, function(m) length(fobj@jobs[[m]]@cmds))))
	return(mat)
}

#' subset_fdef
#' @param fobj flow object
#' @param def flowdef
#' @param start_from where to start from
#'
subset_fdef <- function(fobj, def, start_from){

	if(missing(def))
		stop("Please supply a flow def file")
	mods = names(fobj@jobs)
	mods = mods[which(grepl(start_from, mods)):length(mods)]
	## get mat
	def = subset(def, def$jobname %in% mods)
	def$prev_jobs = ifelse(def$prev_jobs %in% mods, def$prev_jobs, "none")
	def$dep_type = ifelse(def$prev_jobs %in% mods, def$dep_type, "none")

	return(def)
}






