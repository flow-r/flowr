









# setGeneric("submit_flow", function (fobj, ...){
# 	standardGeneric("submit_flow")
# })

#' @rdname submit_flow
#' @title submit_flow
#' @description submit_flow
#' @aliases submit_flow
#' 
#' @param x a \code{object} of class \code{flow}.
#' @param execute \code{logical} whether or not to submit the jobs
#' 
#' @param plot \code{logical} whether to make a pdf flow plot (saves it in the flow working directory).
#' @param uuid \code{character} Advanced use. Incase of rerunning the flow. uuid: typically is the final path of a previous flow.
#' @param verbose logical.
#' @param ... Advanced use. Any additional parameters are passed on to \link{submit_job} function.
#' 
#' 
#' @export
#' @examples
#' \dontrun{
#' submit_flow(fobj = fobj, ... = ...)}
submit_flow <- function(x, ...) {
	message("input x is ", class(x))
	UseMethod("submit_flow")
}

## --- this works when there are a list of fobjs

#' @rdname submit_flow
#' @export
submit_flow.list <- function(x, ...){
	fobjs = lapply(x, submit_flow, ....)
	return(fobjs)
}

#' @rdname submit_flow
#' @export
submit_flow.flow <- function(x, 
	uuid, execute = FALSE,
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
		x@flow_path <- sprintf("%s/%s", x@flow_run_path, uuid)
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
		x@jobs[[i]] <- submit_job(x@jobs[[i]], x, execute=execute, 
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
	try(saveRDS(x, file = sprintf("%s/flow_details.rds", x@flow_path)))
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
