


status_run <- function(x){
	tmp = status(x)
	statuses = sapply(tmp, "[[", "status")
	statuses = factor(statuses, levels = c("complete", "pending", "processing", "errored"))
	summ = as.list(table(statuses))
	return(summ)
}

## name of this function may change
limit_runs <- function(x, max_processing = 7){
	summ = status_run(x)
	
	## limit max number of flows which may be processing at a given time.
	if(summ$processing >= max_processing)
		return(FALSE)
	else
		return(TRUE)
}

#' Submit several flow objects, limit the max running concurrently
#'
#' @param x a list of flow objects
#' @param wd a folder to monitor (flow_run_path)
#' @param max_processing max number of flow which may be processed concurrently
#'
#' @export
submit_run <- function(x, wd, max_processing = 7){
	
	submit_one <- function(x, wd, max_processing){
		while(TRUE){
			runthis = limit_runs(wd, max_processing = max_processing)
			if(runthis){
				submit_flow(x, execute = FALSE)
			}else{ ## sleep for 10 minutes
				Sys.sleep(600)
			}
		}
	}
	
	
	for(i in 1:length(x)){
		submit_one(x[[i]], wd = wd, max_processing = max_processing)
	}

}

if(FALSE){
	
	## a folder which has several flows in it
	x = "/rsrch2/iacs/flowr/runs/sarco/rna_mutect-20150916-00-28-34-V41B7wF2/*"
	
	## let us check the status, and allow only 7 flows to be run
	## at a given time

	## LET fobjs be a list of flow objects to be run
	submit_run(fobj, wd = "<flow_run_path>", max_processing = 7)
	
	## use the devel branch of flowr
	## devtools:::install_github("sahilseth/flowr", ref = "devel")



}