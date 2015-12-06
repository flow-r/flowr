


#' Kill all jobs submitted to the computing platform, for one or multiple flows
#' 
#' 
#' @description 
#' 
#' NOTE:
#' 
#' \strong{This requires files which are created at the end of the \link{submit_flow} command}.
#' 
#' Even if you want to kill the flow, its best to let submit_flow do its job, when done simply use \code{kill(flow_wd)}. 
#' If submit_flow is interrupted, files like flow_details.rds etc are not created, thus flowr looses the association 
#' of jobs with flow instance and cannot monitor, kill or re-run the flow.
#' 
#'
#' @param x either path to flow wd or object of class \link{flow}
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @param kill_cmd The command used to kill. flowr tries to guess this commands, as defined in the detect_kill_cmd(). Supplying
#' it here; fot custom platoforms.
#' @param force You need to set force=TRUE, to kill multiple flows. This makes sure multiple flows are NOT killed by accident.
#' @param ... not used
#' @inheritParams to_flow
#'
#'
#' @export
#' @examples
#'
#' \dontrun{
#' 
#' ## example for terminal
#'## flowr kill_flow x=path_to_flow_directory
#'## In case path matches multiple folders, flowr asks before killing
#'kill(x='fastq_haplotyper*')
#'  Flowr: streamlining workflows
#'  found multiple wds:
#'  /fastq_haplotyper-MS132-20150825-16-24-04-0Lv1PbpI
#'  /fastq_haplotyper-MS132-20150825-17-47-52-5vFIkrMD
#'  Really kill all of them ? kill again with force=TRUE
#'
#'## submitting again with force=TRUE will kill them:
#'kill(x='fastq_haplotyper*', force = TRUE)
#' }
kill <- function(x, ...) {
	UseMethod("kill")
}


#' @rdname kill
#' @importFrom params kable
#' @export
kill.character <- function(x, force = FALSE, ...){
	x = get_wds(x)
	if(length(x) > 1 & !force){
		message("found multiple wds:\n",
						paste(x, collapse = "\n"),
						"\nIf you want to kill all of them, kill again with force=TRUE")
		return("multi wds")
	}
	for(i in 1:length(x)){
		fobj = read_fobj(x[i])
		if(!is.flow(fobj)){
			stop("\nmissing flow_details at this location\n", 
					 "flowr can only kill flows, where the jobs ids are available.\n",
					 "Please check and confirm that the path supplied is correct, ", 
					 "and that it has a flow_details.rds file. \n ls -l ", x[i])
		}
		kill.flow(fobj, ...)
	}
}

#' @rdname kill
#' @importFrom utils txtProgressBar
#' @export
kill.flow <- function(x,
	kill_cmd,
	verbose = opts_flow$get("verbose"),
	jobid_col = "job_sub_id", ...){

	if(missing(kill_cmd)){
		kill_cmd = detect_kill_cmd(x)
	}
	#flow_details = read_flow_detail_fl(wd)

	check_args()
	
	flow_det = to_flowdet(x)

	wd = x@flow_path
	log = file.path(wd, "kill_jobs.out")
	cmds <- sprintf("%s %s >> %s", 
									kill_cmd, flow_det[,jobid_col],
									log)
	
	## redirect STDERR as well if silent
	if(verbose < 2)
		cmds = paste0(cmds, "  2>&1")
	message("killing ", length(cmds), " jobs, please wait... See kill_jobs.out in the wd for more details.")
	
	
	pb <- txtProgressBar(style = 3, min = 1, max = length(cmds))
	tmp <- lapply(1:length(cmds), function(i){
		#for(i in 1:length(cmds)) {
		if(verbose > 2) 
			message(cmds[i], "\n")
		try(system(cmds[i], intern = TRUE))
		if(length(cmds) > 1)
			pb$up(i)
	})
	close(pb)
	
# 	tmp <- pbsapply(cmds, function(cmd){
# 		Sys.sleep(1)
# 		#return(try(system(cmd, intern = TRUE, ...)))
# 		## dots become a problem
# 		## print(as.list(...))
# 		#return(try(system(cmd, intern = TRUE)))
# 	})
	invisible(tmp)
}


#' @importFrom utils tail
detect_stat_cmd <- function(fobj){
	## --- at time first jobs might be local, so fetching from the last
	plat = tail(fobj@jobs, 1)[[1]]@platform
	switch(plat,
		moab = "qstat",
		lsf = "bjobs",
		torque = "qstat",
		sge = "qstat",
		slurm = "sbatch")

}

#' @importFrom utils tail
detect_kill_cmd <- function(fobj){
	## --- at time first jobs might be local, so fetching from the last
	plat = tail(fobj@jobs, 1)[[1]]@platform
	switch(plat,
		moab = "qdel",
		lsf = "bkill",
		torque = "qdel",
		sge = "qdel",
		slurm = "scancel")
}



## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##



kill_flow <- function(...){
	.Deprecated("kill")
	kill(...)
}

