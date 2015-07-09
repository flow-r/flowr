
## --- submit job as part of a flow, this would be called from function flow
#' @rdname submit_job
#' @title .submit_job
#' @description .submit_job
#' @param jobj Object of calls \link{job}
#' @param fobj Object of calls \link{flow}
#' @param execute A \code{logical} vector suggesting whether to submit this job
#' @param verbose logical
#' @param wd working direcotry
#' @param job_id job id
#' @param ... not used
#' @examples \dontrun{
#' .submit_job(jobj = jobj, fobj = fobj, execute = FALSE,
#' verbose = TRUE, wd = wd, job_id = job_id)
#' }
.submit_job <- function (jobj, fobj, execute = FALSE, verbose = FALSE, wd, job_id,...){
	
	## --- get the trigger path
	## --- comes from the flow
	trigger_path=fobj@trigger_path=file.path(fobj@flow_path, "trigger") 
	if(!file.exists(trigger_path)) 
		dir.create(trigger_path, showWarnings=FALSE)
	
	## --- create the name of the job with its index in the supplied flow
	if(!jobj@status %in% c("processed","submitted","running","completed","error"))
		jobj@jobname <- sprintf("%03d.%s", job_id,jobj@name)
	
	## --- get the working dir for these job(s)
	## jobj@name: is indexed
	wd <- file.path(fobj@flow_path, jobj@jobname) 
	if(!file.exists(wd)) 
		dir.create (wd, recursive=TRUE, showWarnings=FALSE);
	
	## --- get the CWD/PWD for all submissions
	jobj@cwd <- file.path(dirname(wd), "tmp") ## FLOWBASE
	
	## --- if serial, MERGE all commands in ONE file
	if(jobj@submission_type %in% c("serial")){
		jobj@cmds <-  paste("## ------", names(jobj@cmds),
			"\n", jobj@cmds, "\n\n", collapse="")
	}
	
	## --- shell scripts and their respective STDOUT/ERR
	files <- sprintf("%s/%s_cmd_%s.sh", wd, jobj@name, 1:length(jobj@cmds))
	## gsub .sh from end of file
	jobj@stderr = jobj@stdout = gsub(".sh$", ".out", files)
	#jobj@stderr <- file.path(wd, jobj@jobname)
	#jobj@stdout <- file.path(wd, jobj@jobname)
	
	## ---- do this for all commands (in case of scatter)
	jobids <- sapply(1:length(jobj@cmds), function(i){
		## ---   make a long job name to capture the run
		obj <- jobj;
		obj@jobname <- sprintf("%s_%s-%s", jobj@jobname,basename(fobj@flow_path),i)
		cmd <- create_queue_cmd(obj, file=files[i], index=i, fobj = fobj)
		
		## ------- make the script; add support for other shells, zsh etc OR detect shell
		beforescript <- c("#!/bin/env bash",
			sprintf("## %s", cmd),
			sprintf("touch %s/trigger/trigger_%s_%s.txt",
				fobj@flow_path, jobj@jobname,i),
			"echo 'BGN at' `date`")
		afterscript <- c(sprintf("exitstat=$?;echo $exitstat > %s/trigger/trigger_%s_%s.txt",
			fobj@flow_path, jobj@jobname,i),
			"echo 'END at' `date`",
			"exit $exitstat") ## returning the exit code
		script <- c(beforescript, jobj@cmds[i], afterscript)
		
		## --- write script to file
		if(verbose) message("Submitting using script:\n", cmd, "\n")
		write(script, files[i])
		
		## --- return CMD if local, else jobid
		if(jobj@platform == "local")
			return(cmd)
		if(execute){
			return(system(cmd, intern = TRUE))
		} ## execute
		return('0') ## if no execute return the 0, as JOBID!
	}) ## for loop
	
	## --- run local and get 0 as jobids
	if(jobj@platform == "local")
		jobids <- run_local(jobids, jobj = jobj, execute = execute)
	#cat("ALERT !! stopping jobs submission. Please don't press Ctrl+C...\n");
	
	## --- Parse jobids
	if(execute)
		jobj@id <- parse_jobids(jobids, platform = jobj@platform)
	
	## --- change the status of this job(s) for logs
	jobj@status <- "processed"
	if(execute) jobj@status <- "submitted"
	
	#Sys.sleep(5);
	return(jobj)
}


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
	cmd <- system(sprintf("echo %s ", jobj@format), intern=TRUE)
	return(cmd=cmd)
}


#' @param file path to the output file
#' @import whisker
create_queue_sh <- function(jobj, index, fobj, ...){

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
	
	## --- get the data to replace in the template
	l <- slots_as_list(jobj, names=slotNames("queue"))
	## --- dependency initially is a list which can have multiple values
	## --- ignore a few of the slots
	l <- l[! names(l) %in% c("format","platform", "dependency")] 
	## --- dependency here is a string according to the policies of the cluster platform
	l <- c(l, dependency=dependency) ## add dependency to the list
	names(l) = toupper(names(l)) ## get list of slots
	l <- c(l, "CMD" = cmd)
	l$STDERR=l$STDOUT=jobj@stdout[index]
	
	
		## find the relevent conf file(s)
	## use the list to replace
	plat_conf = tail(search_conf(x="moab.sh"), 1)
	template <- paste(readLines(plat_conf), collapse = "\n")
	out = whisker.render(template = template, data = l)
	write(x = out, file = fl)
}

