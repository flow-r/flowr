


#' Killing a pipline requires files which are created at the END of the submit_flow commands.
#' 
#' @description 
#' Even if you want to kill the flow, its best to let submit_flow do its job, when done simply use kill(flow_wd). 
#' If submit_flow is interrupted, flow detail files etc are not created, thus flowr can't associate submitted jobs with flow instance.
#' 
#'
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @param force When killing multiple flows, force is neccesary. This makes sure multiple flows are killed by accident.
#' @param ... not used
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
#' @importFrom knitr kable
#' @export
kill.character <- function(x, force = FALSE, ...){
	x = get_wds(x)
	if(length(x) > 1 & !force){
		message("found multiple wds, ",
						kable(x),
						"If you want to kill all of them, kill again with force=TRUE")
		invisible("multi wds")
	}
	for(i in 1:length(x)){
		fobj = read_fobj(x[i])
		kill.flow(fobj, ...)
	}
}

#' @rdname kill
#' @export
kill.flow <- function(x,
	kill_cmd,
	jobid_col = "job_sub_id", ...){

	if(missing(kill_cmd)){
		kill_cmd = detect_kill_cmd(x)
	}
	#flow_details = read_flow_detail_fl(wd)

	flow_det = to_flowdet(x)
	cmds <- sprintf("%s %s", kill_cmd, flow_det[,jobid_col])
	tmp <- sapply(cmds, function(cmd){
		message(cmd, "\n")
		return(system(cmd, intern = TRUE))
	})
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
		slurm = "")

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
		slurm = "")
}



## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##



kill_flow <- function(...){
	.Deprecated("kill")
	kill(...)
}

