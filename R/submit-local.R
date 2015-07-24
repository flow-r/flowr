

## start small, might become more interesting
run_local <- function(x, jobj, cores = 1, execute){
	message("\nWorking on: ", jobj@name)
	if(!execute)
		return(rep(0, length(x)))
	## --- force cores to be 1 IF serial 
	if(jobj@submission_type == "serial")
		cores = 1
	ret <- mclapply(x, system, intern = TRUE, mc.cores = cores)
	return(unlist(ret))
}