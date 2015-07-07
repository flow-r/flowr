## will be extended to flowid later
## x="/scratch/iacs/ngs_runs/*"

#' @export
get_wds <- function(x){
	wds = list.files(dirname(x), full.names = TRUE, pattern = basename(x))
	#wds = list.dirs(x, full.names = TRUE, recursive = FALSE)
	y = file.info(wds)
	rownames(with(y, {subset(y, isdir == TRUE)}))
}

#' @export
get_flow_status <- function(x, cores = 6, out_format = "markdown"){
	
	## --- get all the cmd files
	files_cmd <- list.files(x, pattern = "cmd", full.names = TRUE, recursive = TRUE)
	files_cmd = grep("sh$", files_cmd, value = TRUE)
	## dirname, JOBNAME_cmd_JOBINDEX
	mat_cmd <- data.frame(do.call(rbind,
																strsplit(gsub(".*/(.*)/(.*)_cmd_([0-9]*).sh",
																							"\\1,\\2,\\3", files_cmd), split = ",")),
												file = files_cmd,
												stringsAsFactors = FALSE)
	colnames(mat_cmd) = c("job_id", "job_name", "num", "file")
	#triggers <- sprintf("%s/trigger/trigger_%s_%s.txt", wd, mat_cmd$job_id, mat_cmd$num)
	triggers = sprintf("%s/trigger/trigger_%s_%s.txt", dirname(dirname(files_cmd)),
										 mat_cmd$job_id, mat_cmd$num)
	status <- unlist(mclapply(triggers, function(y){
		if(file.exists(y)){
			tmp <- as.numeric(scan(y, what = "character", quiet = TRUE))
			tmp <- ifelse(length(tmp) > 0, tmp, -1) ## -1 mean not completed
			return(tmp)
		}else
			return(NA)
		#ifelse(length(tmp) < 1 | grepl("Error", tmp), NA, tmp)
	}))
	## STATUS -1 MEANS started
	mat_cmd = data.frame(mat_cmd, started = !is.na(status), status = status)
	flow_mat = try(update_flow_mat(wd = x, mat_cmd = mat_cmd))
	jobs_total <- tapply(mat_cmd$job_id, INDEX = mat_cmd$job_id, length)
	jobs_compl <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, function(z) sum(z > -1, na.rm = TRUE)) ## counts no. more than -1
	jobs_status <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, function(z) sum(ifelse(z>0, 1, 0), na.rm = TRUE))
	jobs_started <- tapply(mat_cmd$started, INDEX = mat_cmd$job_id, function(z) sum(z))
	summ = data.frame(total = jobs_total, started = jobs_started, 
										completed = jobs_compl, exit_status = jobs_status)
	message(paste0("Showing status of: ", x))
	write_flow_status_fl(x = x, summ = summ)
	tmp <- knitr::kable(summ, out_format, output = FALSE)
	print(tmp)
	invisible(flow_mat)
}


#' @title status
#' @description status
#' @aliases status
#' @param x path to the flow; may be without the uuid.
#' @param cores number of cores to use
#' @param out_format passed onto knitr:::kable. supports: markdown, rst, html...
#' @param get_mem_usage under progress, whether to extract mem_usage of jobs
#' @details If x is a path with a single flow, it outputs the status of just that.
#' If the path has more than one flow then this could give a summary of **all** of them.
#' Instead if x is supplied with paths to more than one flow, then this individually prints status of each.
#' @export
#' @importFrom knitr kable
#' @importFrom parallel mclapply
#' @examples
#' \dontrun{
#' status(x = x, cores = 6)
#' ## an example for running from terminal
#' flowr status x=path_to_flow_directory cores=6
#' }
status <- function(x, cores = 6, out_format = "markdown", get_mem_usage = TRUE){
	## get the total jobs
	#wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
	wds = get_wds(x)  
	for(wd in wds){
		get_flow_status(wd, out_format = out_format)
	}
	#return(sum)
}

### provides status of a single flow


## read and update flow_details status
# wd = "/scratch/iacs/iacs_dep/sseth/flows/JZ/telseq/my_super_flow-2015-02-15-20-11-21-UZOwi8Q2"
update_flow_mat <- function(wd, mat_cmd){
	flow_mat = read_flow_detail_fl(wd)
	rownames(flow_mat) = paste(flow_mat$jobname, flow_mat$job_no, sep = "_")
	rownames(mat_cmd) = paste(mat_cmd$job_id, mat_cmd$num, sep = "_")
	flow_mat$started = mat_cmd[rownames(flow_mat), 'started']## track using rownames
	flow_mat$exit_code = mat_cmd[rownames(flow_mat), 'status']## track using rownames
	flow_mat$file = mat_cmd[rownames(flow_mat),'file']
	write_flow_detail_fl(x = wd, flow_mat = flow_mat)
	#   head(flow_mat)
	#   head(mat_cmd)
	#dim(flow_mat)
	invisible(flow_mat)
}




dump_flow_details <- function(fobj){
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

#' kill_flow
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param wd path to a specific which needs to be killed
#' @param fobj a object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @examples 
#' \dontrun{
#' ## example for terminal
#' flowr kill_flow wd=path_to_flow_directory
#' }
#' @export
kill_flow <- function(x, wd, fobj, kill_cmd, 
											jobid_col = "job_sub_id"){
	if(!missing(wd)){
		fobj = read_fobj(wd)
	}
	if(missing(wd)){
		wd = dump_flow_details(fobj)
	}
	if(missing(kill_cmd)){
		kill_cmd = detect_kill_cmd(fobj)
	}
	flow_details = read_flow_detail_fl(wd)
	cmds <- sprintf("%s %s", kill_cmd, flow_details[,jobid_col])
	tmp <- sapply(cmds, function(x){
		message(x, "\n")
		system(x, intern = TRUE)
	})
	invisible(tmp)
}

detect_kill_cmd <- function(fobj){
	## --- at time first jobs might be local, so fetching from the last
	plat = tail(fobj@jobs, 1)[[1]]@platform
	switch(plat, 
				 moab = "qdel",
				 lsf = "bkill",
				 torque = "qdel",
				 sge = "qdel")
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


