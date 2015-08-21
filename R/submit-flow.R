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
#' @param uuid \code{character} Advanced use. This is the final path used for flow execution.
#' Especially useful in case of re-running a flow.
#' @param verbose logical.
#' @param dump dump all the flow details to the flow path
#' @param ... Advanced use. Any additional parameters are passed on to \link{submit_job} function.
#'
#'
#' @export
#' @examples
#' \dontrun{
#' submit_flow(fobj = fobj, ... = ...)}
submit_flow <- function(x, verbose = get_opts("verbose"), ...) {
	if(verbose) message("input x is ", class(x))
	UseMethod("submit_flow")
}

## --- this works when there are a list of fobjs

#' @rdname submit_flow
#' @export
submit_flow.list <- function(x, verbose = get_opts("verbose"), ...){
	fobjs = lapply(x, function(y)
		submit_flow(y, ...)
	)
	return(fobjs)
}

parse_prevjobids <- function(x){

}



#' @rdname submit_flow
#' @param .start_jid Job to start this submission from. Advanced use, should be 1 by default.
#' @importFrom tools file_path_as_absolute
#' @export
submit_flow.flow <- function(x,
														 verbose = get_opts("verbose"),
														 execute = FALSE,
														 uuid,
														 plot = TRUE,
														 dump = TRUE,
														 .start_jid = 1,
														 ...){

	## -- store, for use later
	x@execute=execute

	## --- this field is currently not used extensibly
	## --- Assumption here is that a submitted/processed flow
	## --- has uuid part of its flow_path already
	## the case of resubmission
	if(!file.exists(x@flow_run_path))
		dir.create(x@flow_run_path, recursive = TRUE)

	if(missing(uuid)){
		wd = file.path(file_path_as_absolute(x@flow_run_path), x@desc)
		uuid = get_unique_id(prefix = wd)
	}
	x@flow_path = uuid
	# 		if(!x@status %in%
	# 			 c("processed","submitted","running","completed","exit"))

	##jobnames <- sapply(x@jobs, function(x) x@name)
	##names(x@jobs) <- jobnames
	### ---------- Error handling
	if(execute)
		message("\nFlow is being processed.",
						sprintf(" Track it from R/Terminal using:\nflowr status x=%s\n",
										x@flow_path),
						sprintf("OR from R using:\nstatus(x='%s')\n\n\n",
										x@flow_path))

	## should be included in check flow_def
	if(length(x@jobs[[1]]@dependency_type) > 0 & x@jobs[[1]]@dependency_type !="none")
		stop("Seems like the first job has a dependency, please check")

	## ------   create CWD
	if(!file.exists(file.path(x@flow_path,"tmp")))
		dir.create(file.path(x@flow_path,"tmp"),
							 showWarnings=FALSE, recursive=TRUE)

	## -----   loop on jobs
	## parse dependency from the previous
	## then send it along to submit_job
	## prevjob is null but dep_type exists --- > problem, check should detect.
	## split dependency, if multiple previous jobs

	for(i in .start_jid:length(x@jobs)){
		## ------ check if there are any dependencies
		previous_job <- x@jobs[[i]]@previous_job
		if(verbose) message("Working on, ", i, "with prev: ", previous_job)

		## if there is a previous job
		if(prevjob_exists(previous_job)){
			## --- split multiple dependencies as a list
			## get say a multi column matrix. JOBIDS X PREV JOBS
			previds <- do.call(cbind, lapply(previous_job, function(y)
				x@jobs[[y]]@id))
			## split the MATRIX by rowindex, into a LIST
			x@jobs[[i]]@dependency <- split(previds, row(previds))
		}
	
		## ------ submit the job, get updates job object
		x@jobs[[i]] <- submit_job(jobj = x@jobs[[i]],
															fobj = x,
															execute=execute,
															job_id=i,
															verbose = verbose, ...)
	}

	x@status <- "dry-run"
	if(execute){
		x@status <- "submitted"
	}else{
		message("Test Successful!\n",
						"You may check this folder for consistency. ",
						"Also you may submit again with execute=TRUE\n",
						x@flow_path)
	}

	if(dump){
		flow_det = to_flowdet(x)
		write_flow_details(x@flow_path, fobj = x, plot = plot, flow_det = flow_det)
	}
	invisible(x)
}


prevjob_exists <- function(x){
	if(length(x)!=0){
		## prev job should not be of length 0. need ., NA, "" for missing
		if(!is.na(x[1]) & !is.null(x[1]) & !x[1] %in% c("", "NA", ".", "none", "NULL")){
			return(TRUE)
		}else{
			return(FALSE)
		}
	}else{
		return(FALSE)
	}
}


## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

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
