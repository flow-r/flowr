

## check for .rda files for compatiblity
## then check for f_obj and fobj
read_fobj <- function(x){
	rda = file.path(x, "flow_details.rda")
	rds = file.path(x, "flow_details.rds")
	
	if(file.exists(rda)){
		## attach it:
		tmp <- attach(rda)
		fobj <- get(ls(tmp), tmp)
	}
	
	if(file.exists(rds))
		fobj = readRDS(rds)
	
	return(fobj)
	
}


read_flow_detail_fl <- function(x){
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file))
		stop(error("no.flow_details.file"))
	flow_details = read_sheet(det_file, id_column = "jobname")
	return(flow_details)
}

write_flow_detail_fl <- function(x, flow_mat){
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file)){
		warning(error("perm.flow_details.file"))
		write.table(flow_mat, file = det_file, sep = "\t", quote = FALSE, row.names = FALSE)
	}
	invisible()
}

write_flow_status_fl <- function(x, summ){
	det_file = file.path(x, "flow_details.txt")
	if(!file.exists(det_file)){
		warning(error("perm.flow_status.file"))
		write.table(summ, file.path(x, "flow_status.txt"), quote = FALSE, sep = "\t")
	}
	invisible()
}


