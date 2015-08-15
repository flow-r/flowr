

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

