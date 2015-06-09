

## start small, might become more interesting
run_local <- function(x, j_obj, cores = 1){
	message("\nWorking on ", j_obj@name)
	## --- force cores to be 1 IF serial 
	if(j_obj@submission_type == "serial")
		cores = 1
	ret <- mclapply(x, system, intern = TRUE, mc.cores = cores)
	return(unlist(ret))
}