#' kill_flow
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param wd path to a specific which needs to be killed
#' @param fobj a object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @examples
#' \dontrun{
#' ## example for terminal
#' flowr kill_flow wd=path_to_flow_directory
#' }
#' @export
kill_flow <- function(x, wd, fobj, kill_cmd,
	jobid_col = "job_sub_id"){
		if(!missing(wd)){
			fobj = read_fobj(wd)
		}
		if(missing(wd)){
			wd = dump_flow_details(fobj)
		}
		if(missing(kill_cmd)){
			kill_cmd = detect_kill_cmd(fobj)
		}
		flow_details = read_flow_detail_fl(wd)
		cmds <- sprintf("%s %s", kill_cmd, flow_details[,jobid_col])
		tmp <- sapply(cmds, function(x){
			message(x, "\n")
			system(x, intern = TRUE)
		})
		invisible(tmp)
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
