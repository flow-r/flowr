#' @title create_queue_cmd
#' @description This is a flow interal functions used to create a command used to submit jobs to the cluster.
#' @aliases create_queue_cmd
#' @param jobj object of class \link{job}
#' @param file This is the path to the file to run
#' @param index among cmds defined in \code{jobj}, which index does this \code{file} belong to. A numeric vector of length 1. This is to fetch dependency from previous job.
#' @param ... Not used
#' @keywords internal
#' @examples \dontrun{
#' create_queue_cmd(jobj = jobj, file = file, index = index, ... = ...)
#' }
create_queue_cmd <- function(jobj, file, index, fobj, ...){
	
	if(jobj@platform == "local"){
		cmd <- sprintf("cd %s;%s %s > %s 2>&1;echo 0",
									 jobj@cwd, jobj@submit_exe, file, jobj@stdout[index])
		return(cmd)
	}
	
	## --- get platform of previous job
	prev_plat = try(fobj@jobs[[jobj@previous_job]]@platform, silent = TRUE)
	prev_plat = ifelse(class(prev_plat) == "try-error", "", prev_plat)
	
	## --- this job depends on multiple jobs. 
	## --- create a string with multiple job ids
	## --- introduce possibility that the jobid is empty, 
	## --- or missing especially for reruns
	
	## --- prev LOCAL	OR execute is FALSE
	if(prev_plat == "local" | !fobj@execute){
		dependency <- ""
	}else{
		## --- GATHER
		jobj = jobj
		class(jobj) = jobj@platform
		dependency <- parse_dependency(jobj, index = index)
	}

	## this might be the case if re-run, when only a subset of jobs are to be rerun
	if(length(jobj@dependency) == 0){
		dependency <- ""
	}
	
	l <- slots_as_list(jobj, names=slotNames("queue"))
	## --- dependency initially is a list which can have multiple values
	## --- ignore a few of the slots
	l <- l[! names(l) %in% c("format","platform", "dependency")] 
	## --- dependency here is a string according to the policies of the cluster platform
	l <- c(l, dependency=dependency) ## add dependency to the list
	names(l) = toupper(names(l)) ## get list of slots
	l <- c(l, "CMD" = file)
	l$STDERR=l$STDOUT=jobj@stdout[index]
	
	if(FALSE){ ##finding an alternative to interal call
		## set slots in BASH if we dont use internal they change temporarily
		.Internal(Sys.setenv(names(l), as.character(unlist(l)))) 
	}
	
	## --- send all the arguments to SHELL
	do.call(Sys.setenv, l)
	cmd <- system(sprintf("echo %s ", jobj@format),intern=TRUE)
	return(cmd=cmd)
}


