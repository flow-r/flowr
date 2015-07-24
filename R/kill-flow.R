


#' kill_flow
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param fobj a object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @examples
#' \dontrun{
#' ## example for terminal
#' flowr kill_flow wd=path_to_flow_directory
#' }
#' @export
kill <- function(x, ...) {
	UseMethod("kill")
}

kill.character <- function(x, ...){
	fobj = read_fobj(x)
	kill.flow(x, ...)
}

kill.flow <- function(x, 
	kill_cmd,
	jobid_col = "job_sub_id"){
	
	if(missing(kill_cmd)){
		kill_cmd = detect_kill_cmd(x)
	}
	#flow_details = read_flow_detail_fl(wd)
	
	flow_det = to_flowdet(x)
	cmds <- sprintf("%s %s", kill_cmd, flow_det[,jobid_col])
	tmp <- sapply(cmds, function(cmd){
		message(cmd, "\n")
		system(cmd, intern = TRUE)
	})
	invisible(tmp)
}


kill_flow <- function(...){
	.Deprecated("kill")
	kill(...)
}


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
