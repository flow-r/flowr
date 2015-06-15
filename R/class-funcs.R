






## --- submit job as part of a flow, this would be called from function flow
#' @rdname submit_job
#' @title .submit_job
#' @description submit_job
#' @param j_obj Object of calls \link{job}
#' @param f_obj Object of calls \link{flow}
#' @param execute A \code{logical} vector suggesting whether to submit this job
#' @param verbose logical
#' @param wd working direcotry
#' @param job_id job id
#' @param ... not used
#' @export
#' @examples \dontrun{
#' .submit_job(j_obj = j_obj, f_obj = f_obj, execute = FALSE,
#' verbose = TRUE, wd = wd, job_id = job_id)
#' }
.submit_job <- function (j_obj, f_obj, execute = FALSE, verbose = FALSE, wd, job_id,...){
	
	## --- get the trigger path
	## --- comes from the flow
	trigger_path=f_obj@trigger_path=file.path(f_obj@flow_path, "trigger") 
	if(!file.exists(trigger_path)) 
		dir.create(trigger_path, showWarnings=FALSE)
	
	## --- create the name of the job with its index in the supplied flow
	if(!j_obj@status %in% c("processed","submitted","running","completed","error"))
		j_obj@jobname <- sprintf("%03d.%s", job_id,j_obj@name)
	
	## --- get the working dir for these job(s)
	wd <- file.path(f_obj@flow_path, j_obj@jobname) ## j_obj@name: is indexed
	if(!file.exists(wd)) 
		dir.create (wd, recursive=TRUE, showWarnings=FALSE);
	
	## --- get the CWD/PWD for all submissions
	j_obj@cwd <- file.path(dirname(wd), "tmp") ## FLOWBASE
	
	## --- if serial, MERGE all commands in ONE file
	if(j_obj@submission_type %in% c("serial")){
		j_obj@cmds <-  paste("## ------", names(j_obj@cmds),
												 "\n", j_obj@cmds, "\n\n", collapse="")
	}
	
	## --- shell scripts and their respective STDOUT/ERR
	files <- sprintf("%s/%s_cmd_%s.sh", wd, j_obj@name, 1:length(j_obj@cmds))
	## gsub .sh from end of file
	j_obj@stderr = j_obj@stdout = gsub(".sh$", ".out", files)
	#j_obj@stderr <- file.path(wd, j_obj@jobname)
	#j_obj@stdout <- file.path(wd, j_obj@jobname)
	
	## ---- do this for all commands (in case of scatter)
	jobids <- sapply(1:length(j_obj@cmds), function(i){
		## ---   make a long job name to capture the run
		obj <- j_obj;
		obj@jobname <- sprintf("%s_%s-%s", j_obj@jobname,basename(f_obj@flow_path),i)
		cmd <- create_queue_cmd(obj, file=files[i], index=i, fobj = f_obj)
		
		## ------- make the script; add support for other shells, zsh etc OR detect shell
		beforescript <- c("#!/bin/env bash",
											sprintf("## %s", cmd),
											sprintf("touch %s/trigger/trigger_%s_%s.txt",
															f_obj@flow_path, j_obj@jobname,i),
											"echo 'BGN at' `date`")
		afterscript <- c(sprintf("exitstat=$?;echo $exitstat > %s/trigger/trigger_%s_%s.txt",
														 f_obj@flow_path, j_obj@jobname,i),
										 "echo 'END at' `date`",
										 "exit $exitstat") ## returning the exit code
		script <- c(beforescript, j_obj@cmds[i], afterscript)
		
		## --- write script to file
		if(verbose) message("Submitting using script:\n", cmd, "\n")
		write(script, files[i])
		
		## --- return CMD if local, else jobid
		if(execute){
			if(j_obj@platform == "local")
				return(cmd)
			else
				return(system(cmd, intern = TRUE))
		} ## execute
		return('0') ## if no execute return the 0, as JOBID!
	}) ## for loop
	
	## --- run local and get 0 as jobids
	if(j_obj@platform == "local")
		jobids <- run_local(jobids, j_obj = j_obj)
	#cat("ALERT !! stopping jobs submission. Please don't press Ctrl+C...\n");
	
	## --- Parse jobids
	if(execute)
		j_obj@id <- parse_jobids(jobids, platform = j_obj@platform)
	
	## --- change the status of this job(s) for logs
	j_obj@status <- "processed"
	if(execute) j_obj@status <- "submitted"
	
	#Sys.sleep(5);
	return(j_obj)
}




# setGeneric("submit_flow", function (f_obj, ...){
# 	standardGeneric("submit_flow")
# })

#' @title submit_flow
#' @description submit_flow
#' @aliases submit_flow
#' @param f_obj \code{object} of class \code{flow}.
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
#' submit_flow(f_obj = f_obj, ... = ...)}
submit_flow <- function(f_obj, uuid, execute = FALSE,
												 plot = TRUE, verbose = FALSE, ...){
	## the case of resubmission
	if(missing(uuid)){
		uuid = get_unique_id(f_obj@desc)
	}
	if(!f_obj@status %in% c("processed","submitted","running","completed","exit")){
		f_obj@flow_path <- sprintf("%s/%s",f_obj@flow_base_path, uuid)
	}
	##jobnames <- sapply(f_obj@jobs, function(x) x@name)
	##names(f_obj@jobs) <- jobnames
	### ---------- Error handling
	if(execute)
		message(sprintf("\nFlow is been processed. Track it from terminal using:\nstatus(x='%s')\nOR\nflowr status x=%s\n\n",
									f_obj@flow_path, f_obj@flow_path))
	if(length(f_obj@jobs[[1]]@dependency_type) > 0 & f_obj@jobs[[1]]@dependency_type !="none")
		stop("Seems like the first job has a dependency, please check")
	if(!file.exists(file.path(f_obj@flow_path,"tmp"))) ## create if it does not exist
		dir.create(file.path(f_obj@flow_path,"tmp"), showWarnings=FALSE, recursive=TRUE)
	## loop on jobs
	for(i in 1:length(f_obj@jobs)){
		## ------ check if there are any dependencies
		previous_job <- f_obj@jobs[[i]]@previous_job
		dep_type = f_obj@jobs[[i]]@dependency_type
		if(length(previous_job)!=0){ 
			## prev job should not be of length 0. need ., NA, "" for missing
			## should not be NA OR NULL
			if(!is.na(previous_job[1]) & !is.null(previous_job[1]) & 
				 !previous_job[1] %in% c("", "NA", ".")){
				## f_obj@jobs[[i]]@dependency <- f_obj@jobs[[previous_job]]@id
				## -------- can have multiple dependencies
				x <- do.call(cbind, lapply(previous_job, function(y)
					f_obj@jobs[[y]]@id))
				f_obj@jobs[[i]]@dependency <- split(x, row(x))
				## prev_jobs should have length more than 1. And should not be null
			}else if(length(dep_type) > 0 & !dep_type %in% c("none") & 
							 previous_job %in% c("", "NA", ".")){
				## if prev job is null, but depedency is mentioned
				stop(paste("Previous job name missing for job: ", f_obj@jobs[[i]]@name))
			}
		}
		## ------ submit the job
		f_obj@jobs[[i]] <- .submit_job(f_obj@jobs[[i]], f_obj, execute=execute, job_id=i, 
																	 verbose = verbose, ...)
		## ------ check if this is NOT last job in the flow
		## if(i < length(f_obj@jobs)){
		##     next_job <- f_obj@jobs[[i]]@next_job
		##     if(length(next_job)!=0)     #if we have the next job
		##         f_obj@jobs[[next_job]]@dependency <- f_obj@jobs[[i]]@id
		## }
	}
	f_obj@status <- "processed"
	if(execute){
		f_obj@status <- "submitted"
		## Rscript -e 'flow:::status(\"%s\")
		## dumpt the flow details
	}else{
		message(sprintf("Test Successful!\nYou may check this folder for consistency. Also you may re-run submit with execute=TRUE\n %s",
										f_obj@flow_path))
	}
	try(dump_flow_details(fobj = f_obj))
	try(save(f_obj, file = sprintf("%s/flow_details.rda", f_obj@flow_path)))
	if(plot & length(f_obj@jobs) > 2){
		try(
			.plot_flow(f_obj, detailed = FALSE, pdf = TRUE, type = '1',
								 pdffile = sprintf("%s/%s-flow_design.pdf",f_obj@flow_path, f_obj@name))
		)
	}else{
		if(verbose) message("Skipping plots...\n")
	}
	invisible(f_obj)
}

#setMethod("submit_flow", signature(f_obj = "flow"), definition = .submit_flow)


#### ----------------------- submit loner job
if(FALSE){
	setMethod("submit_job", signature(j_obj = "job"),
						function (j_obj, execute = FALSE,verbose = TRUE, wd, ...){
							## if(verbose) cat(j_obj@base_path, j_obj@name, "\n")
							if(missing(wd)){
								wd <- file.path(j_obj@base_path,paste(j_obj@name,
																											UUIDgenerate(),sep="_"))
							}
							dir.create(wd, recursive=TRUE, showWarnings = FALSE)
							script <- c(j_obj@cmd, sprintf("echo $? > %s/trigger_%s.txt", wd,j_obj@name))
							file <- sprintf("%s/%s.sh", wd, j_obj@name)
							write(script, file)
							j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- wd
							cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file)
							if (verbose) print(cmd)
							if(execute){
								jobid <- system(cmd, intern = TRUE)
								j_obj@id <- jobid
							}
							return(j_obj)
						})
}

## trace("create_queue_cmd", browser, exit=browser, signature = c("queue","character"));
## cmd <- create_queue_cmd(j_obj, file=files[i])
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
## #cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file=files[i])
