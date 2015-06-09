#' @title create_queue_cmd
#' @description This is a flow interal functions used to create a command used to submit jobs to the cluster.
#' @aliases create_queue_cmd
#' @param j_obj object of class \link{job}
#' @param file This is the path to the file to run
#' @param index among cmds defined in \code{j_obj}, which index does this \code{file} belong to. A numeric vector of length 1. This is to fetch dependency from previous job.
#' @param ... Not used
#' @keywords internal
#' @examples \dontrun{
#' .create_queue_cmd(j_obj = j_obj, file = file, index = index, ... = ...)
#' }
.create_queue_cmd <- function(j_obj, file, index, ...){
	
	if(j_obj@platform == "local"){
		cmd <- sprintf("cd %s;%s %s > %s 2>&1;echo 0",
									 j_obj@cwd, j_obj@submit_exe, file, j_obj@stdout[index])
		return(cmd)
	}
	
	## --- this job depends on multiple jobs. 
	## --- create a string with multiple job ids
	## --- introduce possibility that the jobid is empty, 
	## --- or missing especially for reruns
	
	## --- GATHER
	if(j_obj@dependency_type=="gather"){
		## dependency may be a list with few elements: multi jobs
		## multiple lists with length 1 each
		## easiest to unlist, and WAIT for ALL of them
		if(j_obj@platform=="torque")
			dependency <- sprintf("-W depend=afterok:%s", 
														paste(unlist(j_obj@dependency), collapse = ":"))
		else if(j_obj@platform=="lsf")
			#dependency <- sprintf("-w '%s'", paste(j_obj@dependency, collapse=" && "))
			dependency <- sprintf("-w '%s'", 
														paste(unlist(j_obj@dependency), collapse = " && "))
		
		## --- SERIAL
	}else if (j_obj@dependency_type=="serial"){
		## if submission is scatter and dependency is serial, do burst
		## basically recycle the dependency for all the subsequent jobs
		if(length(j_obj@dependency)==1) index=1 ## recycle the first into the rest
		if(j_obj@platform=="torque")
			dependency <- sprintf("-W %s",paste(" depend=afterok:",
																					j_obj@dependency[[index]], 
																					sep="", collapse=":"))
		else if(j_obj@platform=="lsf")
			dependency <- sprintf("-w '%s'", paste(j_obj@dependency[[index]], 
																						 collapse=" && "))
		
		## --- BURST
	}else if (j_obj@dependency_type=="burst"){
		## if submission is scatter and dependency is serial, do burst
		index=1
		if(j_obj@platform=="torque")
			dependency <- sprintf("-W %s",paste(" depend=afterok:", 
																					j_obj@dependency[[index]], sep="",
																					collapse=":"))
		else if(j_obj@platform=="lsf")
			dependency <- sprintf("-w '%s'", paste(j_obj@dependency[[index]],
																						 collapse=" && "))
	}else{
		dependency <- ""
	}
	
	## this might be the case if re-run, when only a subset of jobs are to be rerun
	if(length(j_obj@dependency) == 0){
		dependency <- ""
	}
	
	l <- slots_as_list(j_obj, names=slotNames("queue"))
	## --- dependency initially is a list which can have multiple values
	## --- ignore a few of the slots
	l <- l[! names(l) %in% c("format","platform", "dependency")] 
	## --- dependency here is a string according to the policies of the cluster platform
	l <- c(l, dependency=dependency) ## add dependency to the list
	names(l) = toupper(names(l)) ## get list of slots
	l <- c(l, "CMD" = file)
	l$STDERR=l$STDOUT=j_obj@stdout[index]
	
	if(FALSE){ ##finding an alternative to interal call
		## set slots in BASH if we dont use internal they change temporarily
		.Internal(Sys.setenv(names(l), as.character(unlist(l)))) 
	}
	
	## --- send all the arguments to SHELL
	do.call(Sys.setenv, l)
	cmd <- system(sprintf("echo %s ", j_obj@format),intern=TRUE)
	return(cmd=cmd)
}