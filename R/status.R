## will be extended to flowid later
## x="/scratch/iacs/ngs_runs/*"

#' Get all the (sub)directories in a folder
#' @param x path to a folder
#' @export
get_wds <- function(x){
	wds = list.files(dirname(x), full.names = TRUE, pattern = basename(x))
	#wds = list.dirs(x, full.names = TRUE, recursive = FALSE)
	y = file.info(wds)
	rownames(with(y, {subset(y, isdir == TRUE)}))
}


#' @title status
#' @description Summarize status of executed flow(x)
#' @aliases status
#'
#' @param x path to the flow root folder or a parent folder to summarize several flows.
#' @param out_format passed onto knitr:::kable. supports: markdown, rst, html...
#'
#' @details
#' basename(x) is used in a wild card search.
#'
#' \itemize{
#' \item If x is a path with a single flow, it outputs the status of one flow.
#' \item If the path has more than one flow then this could give a summary of **all** of them.
#' \item Instead if x is supplied with paths to more than one flow, then this individually prints status of each.
#' }
#'
#' Alternatively, x can also be a flow object
#'
#' @export
#'
#' @importFrom parallel mclapply
#'
#' @examples
#' \dontrun{
#' status(x = "~/flowr/runs/sleep_pipe*")
#' ## an example for running from terminal
#' flowr status x=path_to_flow_directory cores=6
#' }
status <- function(x, out_format = "markdown"){
	## get the total jobs
	#wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
	wds = get_wds(x)
	for(wd in wds){
		x = read_fobj(wd)
		get_status(x, out_format = out_format)
	}
	invisible()
}

#' @rdname status
#'
#' @param ... not used
#'
#' @export
get_status <- function(x, ...) {
	UseMethod("get_status")
}

#' @rdname status
#' @export
get_status.character <- function(x, out_format = "markdown", ...){
	## Get a shorter get_status
	flow_det = to_flowdet.rootdir(x)
	flow_det = get_status(flow_det)
	summ = summarize_flow_det(flow_det)
	write_flow_details(x, summ = summ)
}

#' @rdname status
#' @export
get_status.data.frame <- function(x, ...){

	exit_code <- unlist(lapply(x$trigger, function(y){
		if(file.exists(y)){
			tmp <- as.numeric(scan(y, what = "character", quiet = TRUE))
			tmp <- ifelse(length(tmp) > 0, tmp, -1) ## -1 mean not completed
			return(tmp)
		}else
			return(NA)
	}))

	x$started = !is.na(exit_code)
	x$exit_code = exit_code

	return(x)
}


#' @rdname status
#' @export
get_status.flow <- function(x, out_format = "markdown", ...){

	## --- get initial flow_det from the flow object
	flow_det = to_flowdet(x)
	## --- update the flow_det using the triggers
	flow_det = get_status(flow_det)

	message(paste0("Showing status of: ", x@flow_path))
	#write_flow_status_fl(x = x, summ = summ)
	summ = summarize_flow_det(flow_det, out_format = out_format)

	## -- updating flow object
	for(i in nrow(summ)){
		jobnm = summ$jobnm[i]
		status = summ$status[i]
		x@jobs[[jobnm]]@status = status
	}
	## --- if input is a flow
	## update status and exit code in the flow object

	## write out fobj, status and flow_det
	write_flow_details(x = x@flow_path, summ = summ, flow_det = flow_det)

	#flow_det = try(update_flow_det(wd = x@flow_path, mat_cmd = mat_cmd))
	invisible(flow_det)
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
		if(diff == 0){
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
	tmp <- knitr::kable(summ, out_format, output = FALSE)
	print(tmp)
	summ = cbind(jobname = rownames(summ), jobnm = nm, summ)
	return(summ)
}


### provides status of a single flow
## read and update flow_details status
# wd = "/scratch/iacs/iacs_dep/sseth/flows/JZ/telseq/my_super_flow-2015-02-15-20-11-21-UZOwi8Q2"










## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

#' update_flow_det
#' @param wd flow working directory
#' @param mat_cmd a table with details about cmd files
#' @details
#'
#' Get the flow_det files from wd, and update it with new statuses.
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


