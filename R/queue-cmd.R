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
.create_queue_cmd <- function(j_obj, file, index, fobj, ...){
	
	if(j_obj@platform == "local"){
		cmd <- sprintf("cd %s;%s %s > %s 2>&1;echo 0",
									 j_obj@cwd, j_obj@submit_exe, file, j_obj@stdout[index])
		return(cmd)
	}
	
	## --- get platform of previous job
	prev_plat = fobj@jobs[[j_obj@previous_job]]@platform
	
	## --- this job depends on multiple jobs. 
	## --- create a string with multiple job ids
	## --- introduce possibility that the jobid is empty, 
	## --- or missing especially for reruns
	
	## --- prev LOCAL	
	if(prev_plat == "local"){
		dependency <- ""
	}else{
		## --- GATHER
		jobj = j_obj
		class(jobj) = jobj@platform
		dependency <- parse_dependency(jobj, index = index)
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


