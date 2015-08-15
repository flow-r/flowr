


#' kill
#'
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @param ... not used
#'
#'
#' @export
#' @examples
#'
#' \dontrun{
#' ## example for terminal
#' ## flowr kill_flow x=path_to_flow_directory
#' }
kill <- function(x, ...) {
	UseMethod("kill")
}


#' @rdname kill
#' @description works on flow_path. Reads flow object and calls kill.flow()
#' @export
kill.character <- function(x, ...){
	for(i in 1:length(x)){
		fobj = read_fobj(x[i])
		kill.flow(fobj, ...)
	}
}

#' @rdname kill
#' @description works on flow object
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

