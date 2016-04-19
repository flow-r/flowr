

## check for .rda files for compatiblity
## then check for f_obj and fobj


# read flow object given a flow execution folder
# @param x path to a flow execution folder
#
# @return
# if it finds a fobj, returns that. If not return back the path x
read_fobj <- function(x){
	rda = file.path(x, "flow_details.rda")
	rds = file.path(x, "flow_details.rds")

	if(file.exists(rda)){
		message("Status on these functions, may not work with later versions.")
		## attach it:
		#tmp <- attach(rda)
		#fobj <- get(ls(tmp), tmp)
	}else if(file.exists(rds))
		fobj = readRDS(rds)
	else
		return(x) ## just return the WD

	return(fobj)
}



#' write files desribing this flow
#'
#' @param x path to write to
#' @param fobj flow object
#' @param summ a status summary.
#' @param flow_det a flow details data.frame
#' @param flow_mat flow matrix (of commands)
#' @param flow_def flow definiion
#' @param plot logical, plot or not
write_flow_details <- function(x, 
                               fobj, 
                               summ, 
                               flow_det, 
                               flow_mat,
                               flow_def,
                               plot = FALSE){

	if(!missing(summ))
		try(write_sheet(summ, file.path(x, "flow_status.txt"), ext = "tsv"), silent = TRUE)

	if(!missing(flow_det))
		try(write_sheet(flow_det, file.path(x, "flow_details.txt"), ext = "tsv"), silent = TRUE)

	if(!missing(fobj))
		try(saveRDS(fobj, file = file.path(x, "flow_details.rds")), silent = TRUE)

  if(!missing(flow_mat))
    try(write_sheet(flow_mat, file.path(x, "flow_details.mat"), ext = "tsv"), silent = TRUE)

  if(!missing(flow_def))
    try(write_sheet(flow_def, file.path(x, "flow_details.def"), ext = "tsv"), silent = TRUE)
  
  if(plot & !missing(fobj)){
		if(length(fobj@jobs) > 2)
			try(
				plot_flow(fobj, detailed = TRUE, pdf = TRUE, type = '1',
					pdffile = file.path(x, "flow_design.pdf")), silent = TRUE)
		else
			message("Skipping plots...")
	}
}





## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

read_flow_detail_fl <- function(x){
	.Deprecated("to_flowdet")
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file))
		stop(error("no.flow_details.file"))
	flow_details = read_sheet(det_file, id_column = "jobname")
	return(flow_details)
}

write_flow_detail_fl <- function(x, flow_det){
	.Deprecated("write_flow_details")
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file)){
		warning(error("perm.flow_details.file"))
		write.table(flow_det, file = det_file, sep = "\t", quote = FALSE, row.names = FALSE)
	}
	invisible()
}

write_flow_status_fl <- function(x, summ){
	.Deprecated("write_flow_details")
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file)){
		warning(error("perm.flow_status.file"))
		write.table(summ, file.path(x, "flow_status.txt"), quote = FALSE, sep = "\t")
	}
	invisible()
}

dump_flow_helpers <- function(x, plot, verbose){

	.Deprecated("write_flow_details")
	## should update instead of overwrite.
	try(write_flow_details(fobj = x))

	try(saveRDS(x, file = sprintf("%s/flow_details.rds", x@flow_path)))

	if(plot & length(x@jobs) > 2){
		try(
			plot_flow(x, detailed = FALSE, pdf = TRUE, type = '1',
								pdffile = sprintf("%s/flow_design.pdf",
																	x@flow_path))
		)
	}else{
		if(verbose) message("Skipping plots...\n")
	}
}

