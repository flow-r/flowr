## will be extended to flowid later
## x="/scratch/iacs/ngs_runs/*"

#' Get all the (sub)directories in a folder
#' @param x path to a folder
#' @export
get_wds <- function(x){
	wds = list.files(dirname(x), full.names = TRUE, pattern = basename(x))
	#wds = list.dirs(x, full.names = TRUE, recursive = FALSE)
	y = file.info(wds)

	## if the current folder contains a rds file
	if(file.exists(file.path(x, "flow_details.rds")))
		return(x)
	
	dirs = rownames(with(y, {subset(y, isdir == TRUE)}))
	return(dirs)
}


#' @title Monitor status of flow(s)
#' 
#' @description 
#' Summarize status of a flow OR multiple flows OR a high-level summary of all flows in a folder.
#' 
#' 
#' @aliases status
#'
#' @param x path to the flow root folder or a parent folder to summarize several flows.
#' @param use_cache This skips checking status of jobs which have already been completed a
#' and assumes no new jobs were submitted in the flow(s) being monitored. [FALSE]
#' @param out_format passed onto knitr:::kable. supports: markdown, rst, html... [markdown]
#' @param progress Whether or not to show a progress bar, when fetching/reading files [TRUE]
#' @inheritParams to_flow
#'
#' @details
#' basename(x) is used in a wild card search.
#'
#' \itemize{
#'   \item Get status of all the flows: 
#'   (all flows with 'sleep_pipe' in their name are checked and their status is shown)
#'   <br>
#'   \code{flowr status x=~/flowr/runs/sleep_pipe*}
#'   \item Provide a high level summary of ALL flows in a folder:
#'   <br>
#'    \code{flowr status x=~/flowr/runs}
#' }
#'
#' Use \strong{use_cache}=TRUE to speed up checking the status. 
#' This assumes that no new jobs have been submitted and skips (re-)checking status of 
#' completed jobs.
#' 
#' Once all the jobs have been submitted to the cluster you may always use \code{use_cache=TRUE}.
#' 
#' @export
#' @importFrom parallel mclapply
#'
#' @examples
#' \dontrun{
#' status(x = "~/flowr/runs/sleep_pipe*")
#' ## an example for running from terminal
#' flowr status x=path_to_flow_directory
#' }
status <- function(x, 
									 use_cache = FALSE,
									 verbose = opts_flow$get("verbose"),
									 out_format = "markdown", ...){
	## get the total jobs
	#wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
	
	if(missing(x))
		stop("Please provide a path to a flow wd. x='my-awesome-flowpath'")
	
	# if a flow object it specified
	if(is.flow(x)){
	  return(get_status(x, out_format = out_format, verbose = verbose, use_cache = use_cache, ...))
	}
	
	wds = get_wds(x)
	lst = lapply(wds, function(wd){
		
		ncol = getOption("width"); #Sys.getenv("COLUMNS")
		hd = paste(rep("=", as.numeric(ncol)), collapse = "")
		message(paste0("\n", hd,
									 "\nSummarizing status (using triggers) of: \n", wd))
		if(use_cache)
			message("Using cache for speed, skipping checking jobs, which were previously marked complete...")
		
		x = read_fobj(wd)
		lst = get_status(x, out_format = out_format, verbose = verbose, use_cache = use_cache, ...)
	})
	invisible(lst)
}

#' @rdname status
#'
#' @param ... not used
#'
#' @export
get_status <- function(x, ...) {
	
	UseMethod("get_status")
}


final_status <- function(x){
	#statuses = sapply(x@jobs, function(y) y@status)
	statuses = x
	if(all(statuses == "completed")){ ## all complete
		final = "complete"
	}else if(any(statuses == "errored")){ ## some errored
		final = "errored"
	}else if(all(statuses == "pending")){ ## all pending
		final = "pending"
	}else if(any(statuses == "processing")){ ## some processing
		final = "processing"
	}else if(any(statuses == "pending")){ ## some pending
		final = "processing"
	}
	return(final)
}


#' @rdname status
#' @export
get_status.flow <- function(x, verbose, use_cache, out_format, ...){

	## --- get initial flow_det from the flow object
	flow_det = to_flowdet(x)
	## --- update the flow_det using the triggers
	flow_det = get_status(flow_det, verbose, use_cache, ...)
	
	#write_flow_status_fl(x = x, summ = summ)
	summ = summarize_flow_det(flow_det, out_format = out_format)
	
	## -- updating flow object
	for(i in nrow(summ)){
		jobnm = summ$jobnm[i]
		status = summ$status[i]
		x@jobs[[jobnm]]@status = status
	}
	status = final_status(summ$status)
	x@status = status
	## --- if input is a flow
	## update status and exit code in the flow object
	
	## write out fobj, status and flow_det
	write_flow_details(x = x@flow_path, summ = summ, flow_det = flow_det)
	
	#flow_det = try(update_flow_det(wd = x@flow_path, mat_cmd = mat_cmd))
	invisible(list(summary = summ, status = status))
}

#' @rdname status
#' @export
get_status.character <- function(x, verbose, use_cache, out_format, ...){
	
	## Get a shorter get_status
	## this is a summarizing of several flows, 
	## this would be especially slow at times.
	if(verbose > 1)
		message("getting to_flowdet")
	fl = file.path(x, "flow_details.txt")
	
	if(use_cache == TRUE & file.exists(fl)){
		flow_det = read_sheet(fl);
		if(verbose > 1)
			message("skipped to_flowdet")
		
	}else{
	  # if x is a character
		flow_det = to_flowdet.rootdir(x)
	}
	
	flow_det = get_status(flow_det, verbose, use_cache, ...)
	summ = summarize_flow_det(flow_det, out_format = out_format)
	status = final_status(summ$status)
	write_flow_details(x, summ = summ, flow_det = flow_det)
	invisible(list(summary = summ, status = status))
}

#' @rdname status
#' @export
get_status.data.frame <- function(x, verbose, use_cache, progress = TRUE, ...){
	
	## get exit codes for all triggers
	## got over each row of flowdet and work through it
	## one may need to make this faster, for now, we will just show a progress bar
	if(verbose > 1)
		message("fetching exit codes...")
	
	# eval is we need to show progress
	show_progress = all(progress, nrow(x) > 1)
	if(show_progress)
		pb <- txtProgressBar(min = 1, max = nrow(x), style = 3)
	
	get_code <-  function(i){
		if(show_progress)
			pb$up(i)
		fl = x$trigger[i]
		## skip reading the code
		code = x$exit_code[i]
		code = ifelse(is.null(code), -1, code)
		code = ifelse(is.na(code), -1, code)
		if(verbose > 2)
			message("previous exit code: ", code)
		if( code == 0 & use_cache){
			return(0)
		}
		if(file.exists(fl)){
			tmp <- as.numeric(scan(fl, what = "character", quiet = TRUE))
			code <- ifelse(length(tmp) > 0, tmp, -1) ## -1 mean not completed
			return(code)
		}else{
			return(NA)
		}
	}
	
	exit_code <- lapply(1:nrow(x), get_code)
	exit_code = unlist(exit_code)
	
	if(show_progress)
		close(pb)
	
	x$started = !is.na(exit_code)
	x$exit_code = exit_code
	
	return(x)
}




## depreciated
.summarize_flow_det <- function(x, out_format){
	.Deprecated("summarize_flow_det")
	## summarize
	nm <- tapply(x$jobnm, INDEX = x$jobname, unique)
	jobs_total <- tapply(x$jobname, INDEX = x$jobname, length)
	jobs_compl <- tapply(x$exit_code, INDEX = x$jobname,
											 function(z) sum(z > -1, na.rm = TRUE)) ## counts no. more than -1
	jobs_status <- tapply(x$exit_code, INDEX = x$jobname, function(z) sum(ifelse(z>0, 1, 0), na.rm = TRUE))
	jobs_started <- tapply(x$started, INDEX = x$jobname, function(z) sum(z))
	summ = data.frame(total = jobs_total, started = jobs_started,
										completed = jobs_compl, exit_status = jobs_status, stringsAsFactors = FALSE)
	status = sapply(1:nrow(summ), function(i){
		diff = summ$total[i] - summ$completed[i]
		if(diff == 0){
			return("completed")
		}else if(diff < "summ$total[i]"){
			return("processing")
		}else{
			return("waiting")
		}
	})
	summ$status = status
	tmp <- params::kable(summ, out_format, output = FALSE)
	message(paste(tmp, collapse = "\n"))
	summ = cbind(jobname = rownames(summ), jobnm = nm, summ)
	return(summ)
}

summarize_flow_det <- function(x, out_format){
	## summarize
	nm <- tapply(x$jobnm, INDEX = x$jobname, unique)
	jobs_total <- tapply(x$jobname, INDEX = x$jobname, length)
	jobs_compl <- tapply(x$exit_code, INDEX = x$jobname,
											 function(z) sum(z > -1, na.rm = TRUE)) ## counts no. more than -1
	jobs_status <- tapply(x$exit_code, INDEX = x$jobname, function(z) sum(ifelse(z>0, 1, 0), na.rm = TRUE))
	jobs_started <- tapply(x$started, INDEX = x$jobname, function(z) sum(z))
	summ = data.frame(total = jobs_total, started = jobs_started,
										completed = jobs_compl, exit_status = jobs_status, stringsAsFactors = FALSE)
	status = sapply(1:nrow(summ), function(i){
		diff = summ$total[i] - summ$completed[i]
		if(summ$exit_status[i] > 0){
			return("errored")
		}else if(diff == 0){
			return("completed")
		}else if (summ$started[i] == 0){
			return("pending")
		}else if(summ$started[i] > 0){
			return("processing")
		}else{
			return("")
		}
	})
	summ$status = status
	tmp <- params::kable(summ, out_format, output = FALSE)
	message(paste(tmp, collapse = "\n"))
	summ = cbind(jobname = rownames(summ), jobnm = nm, summ)
	return(summ)
}


### provides status of a single flow
## read and update flow_details status
# wd = "/scratch/iacs/iacs_dep/sseth/flows/JZ/telseq/my_super_flow-2015-02-15-20-11-21-UZOwi8Q2"










## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

# update_flow_det
# @param wd flow working directory
# @param mat_cmd a table with details about cmd files
#
# @details
# Get the flow_det files from wd, and update it with new statuses.
update_flow_det <- function(wd, mat_cmd){
	.Deprecated("to_flowdet")
	flow_det = read_flow_detail_fl(wd)
	rownames(flow_det) = paste(flow_det$jobname, flow_det$job_no, sep = "_")
	rownames(mat_cmd) = paste(mat_cmd$job_id, mat_cmd$num, sep = "_")
	flow_det$started = mat_cmd[rownames(flow_det), 'started']## track using rownames
	flow_det$exit_code = mat_cmd[rownames(flow_det), 'status']## track using rownames
	flow_det$file = mat_cmd[rownames(flow_det),'file']
	write_flow_detail_fl(x = wd, flow_det = flow_det)
	#   head(flow_det)
	#   head(mat_cmd)
	#dim(flow_det)
	invisible(flow_det)
}


#' @importFrom utils write.table
create_flow_det <- function(fobj){
	.Deprecated("to_flowdet")
	ret <- lapply(1:length(fobj@jobs), function(i){
		ids = fobj@jobs[[i]]@id ## jobid for submission
		deps = fobj@jobs[[i]]@dependency
		deps = sapply(deps, paste, collapse = ";")
		prev = fobj@jobs[[i]]@previous_job ## works for single type jobs
		prev = paste(prev, collapse = ";")
		#ifelse(prev != "") prev = paste(prev, 1:length(fobj@jobs[[prev]]@id), sep = "_")
		job_no = 1:length(ids)
		job_id = paste(fobj@jobs[[i]]@jobname, job_no, sep = "_")
		mat = cbind(jobname = fobj@jobs[[i]]@jobname,
								jobnm = fobj@jobs[[i]]@name,
								job_no = job_no, job_sub_id = ids,
								job_id = job_id,prev = prev,
								dependency = ifelse(is.null(unlist(deps)), NA, unlist(deps)),
								status = fobj@jobs[[i]]@status, exit_code = NA)
	})
	flow_details = do.call(rbind, ret)
	write.table(flow_details, sep = "\t", quote = FALSE, row.names = FALSE,
							file = sprintf("%s/flow_details.txt",fobj@flow_path, fobj@name))
	return(file.path(fobj@flow_path))
}



if(FALSE){
	x = "/scratch/iacs/ngs_runs/140917_SN746_0310_AC5GKGACXX/logs/"
	# Rscript -e 'library(flow); get_flow_status("shrna-Z1-2400REF1-ca2caca1-c484-4034-8f7c-d9b07c595095")'
	library(flow);
	setwd("/scratch/iacs/iacs_dep/sseth/flows/TH/MP_Melanoma_PM70")
	#debug(get_flow_status)
	get_flow_status("shrna-2400KinPlusbaseline-ref1")
	## Rscript -e 'flow:::status("log")'
}


