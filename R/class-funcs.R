






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




# setGeneric("submit_flow", function (fobj, ...){
# 	standardGeneric("submit_flow")
# })

#' @title submit_flow
#' @description submit_flow
#' @aliases submit_flow
#' @param fobj \code{object} of class \code{flow}.
#' @param uuid \code{character} A character string pointing to the folder (unique) where all the logs and other files are processed. This is optional and defaults to:
#'  \code{FLOW_DESCRIPTION_UUID}, and this folder is typically created in \code{~/flows/FLOW_NAME}.
#'  Refer to \code{desc} and \code{name} paramters of \link{flow}.
#' @param execute \code{logical} whether or not to submit the jobs
#' @param plot \code{logical} whether to make a flow plot (saves it in the flow working directory)
#' @param verbose logical.
#' @param ... Any additional parameter are passed on to \link{submit_job} function
#' @export
#' @examples
#' \dontrun{
#' submit_flow(fobj = fobj, ... = ...)}
submit_flow <- function(x, ...) {
	message("input x is ", class(x))
	UseMethod("submit_flow")
}

## --- this works when there are a list of fobjs

#' @export
submit_flow.list <- function(x, ...){
	fobjs = lapply(x, submit_flow, ....)
	return(fobjs)
}

#' @export
submit_flow.flow <- function(x, uuid, execute = FALSE,
												 plot = TRUE, verbose = FALSE, ...){
	## -- store, for use later
	x@execute=execute
	## the case of resubmission
	if(missing(uuid)){
		uuid = get_unique_id(x@desc)
	}
	
	## --- this field is currently not used extensibly
	## --- Assumption here is that a submitted/processed flow 
	## --- has uuid part of its flow_path already
	if(!x@status %in% c("processed","submitted","running","completed","exit")){
		x@flow_path <- sprintf("%s/%s", x@flow_base_path, uuid)
	}
	##jobnames <- sapply(x@jobs, function(x) x@name)
	##names(x@jobs) <- jobnames
	### ---------- Error handling
	if(execute)
		message(sprintf("\nFlow is being processed. Track it from R/Terminal using:\nstatus(x='%s')\nOR\nflowr status x=%s\n\n",
									x@flow_path, x@flow_path))
	if(length(x@jobs[[1]]@dependency_type) > 0 & x@jobs[[1]]@dependency_type !="none")
		stop("Seems like the first job has a dependency, please check")
	## create if it does not exist
	if(!file.exists(file.path(x@flow_path,"tmp"))) 
		dir.create(file.path(x@flow_path,"tmp"), 
							 showWarnings=FALSE, recursive=TRUE)
	## loop on jobs
	for(i in 1:length(x@jobs)){
		## ------ check if there are any dependencies
		previous_job <- x@jobs[[i]]@previous_job
		dep_type = x@jobs[[i]]@dependency_type
		if(length(previous_job)!=0){ 
			## prev job should not be of length 0. need ., NA, "" for missing
			## should not be NA OR NULL
			if(!is.na(previous_job[1]) & !is.null(previous_job[1]) & 
				 !previous_job[1] %in% c("", "NA", ".")){
				## x@jobs[[i]]@dependency <- x@jobs[[previous_job]]@id
				## -------- can have multiple dependencies
				previds <- do.call(cbind, lapply(previous_job, function(y)
					x@jobs[[y]]@id))
				x@jobs[[i]]@dependency <- split(previds, row(previds))
				## prev_jobs should have length more than 1. 
				## And should not be null
			}else if(length(dep_type) > 0 & !dep_type %in% c("none") & 
							 previous_job %in% c("", "NA", ".")){
				## if prev job is null, but depedency is mentioned
				stop(paste("Previous job name missing for job: ", 
									 x@jobs[[i]]@name))
			}
		}
		## ------ submit the job
		x@jobs[[i]] <- .submit_job(x@jobs[[i]], x, execute=execute, 
															 job_id=i, verbose = verbose, ...)
		## ------ check if this is NOT last job in the flow
		## if(i < length(x@jobs)){
		##     next_job <- x@jobs[[i]]@next_job
		##     if(length(next_job)!=0)     #if we have the next job
		##         x@jobs[[next_job]]@dependency <- x@jobs[[i]]@id
		## }
	}
	x@status <- "processed"
	if(execute){
		x@status <- "submitted"
		## Rscript -e 'flow:::status(\"%s\")
		## dumpt the flow details
	}else{
		message(sprintf("Test Successful!\nYou may check this folder for consistency. Also you may re-run submit with execute=TRUE\n %s",
										x@flow_path))
	}
	try(dump_flow_details(fobj = x))
	try(saveRDS(x, file = sprintf("%s/flow_details.rda", x@flow_path)))
	if(plot & length(x@jobs) > 2){
		try(
			plot_flow(x, detailed = FALSE, pdf = TRUE, type = '1',
								 pdffile = sprintf("%s/%s-flow_design.pdf", 
								 									x@flow_path, x@name))
		)
	}else{
		if(verbose) message("Skipping plots...\n")
	}
	invisible(x)
}

#setMethod("submit_flow", signature(fobj = "flow"), definition = .submit_flow)


#### ----------------------- submit loner job
if(FALSE){
	setMethod("submit_job", signature(jobj = "job"),
						function (jobj, execute = FALSE,verbose = TRUE, wd, ...){
							## if(verbose) cat(jobj@base_path, jobj@name, "\n")
							if(missing(wd)){
								wd <- file.path(jobj@base_path,paste(jobj@name,
																											UUIDgenerate(),sep="_"))
							}
							dir.create(wd, recursive=TRUE, showWarnings = FALSE)
							script <- c(jobj@cmd, sprintf("echo $? > %s/trigger_%s.txt", wd,jobj@name))
							file <- sprintf("%s/%s.sh", wd, jobj@name)
							write(script, file)
							jobj@stderr <- wd;jobj@stdout <- wd;jobj@cwd <- wd
							cmd <- sprintf("%s %s",create_queue_cmd(jobj), file)
							if (verbose) print(cmd)
							if(execute){
								jobid <- system(cmd, intern = TRUE)
								jobj@id <- jobid
							}
							return(jobj)
						})
}

## trace("create_queue_cmd", browser, exit=browser, signature = c("queue","character"));
## cmd <- create_queue_cmd(jobj, file=files[i])
## untrace("create_queue_cmd", signature = c("queue","character"));

## setMethod("create_queue_cmd", signature(q_obj = "queue"), function (q_obj, ...){
##     if(q_obj@dependency_type=="gather"){
##         if(q_obj@platform=="torque")
##             q_obj@dependency <- sprintf("-W depend=afterok:%s",paste(q_obj@dependency, collapse=":"))
##         else if(q_obj@platform=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",paste(q_obj@dependency, sep=" && "))
##     }else if (q_obj@dependency_type=="serial"){
##         if(q_obj@platform=="torque")
##             q_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",q_obj@dependency[index], sep=""))
##         else if(q_obj@platform=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",q_obj@dependency[index])
##     }else{
##         q_obj@dependency <- ""
##     }
##     l <- slots_as_list(q_obj, names=slotNames("queue"))
##     l <- l[! names(l) %in% c("format","platform")] ### ignore a few of the slots
##     names(l) = toupper(names(l)) ## get list of slots
##     ## l <- c("CMD"=cmd)
##     .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
##     cmd <- system(sprintf("eval echo %s ",q_obj@format),intern=TRUE)
##     return(cmd=cmd)
## })
## #cmd <- sprintf("%s %s",create_queue_cmd(jobj), file=files[i])
