## Needs some work
## in future if output of this is made similar to flowdef,
## plot_flow could direclty work on flowdef.

# nocov start

## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

#' @title create_jobs_mat
#' @description create_jobs_mat
#' @param x a \link{flow} object.
#' @keywords internal
#' @details This create a table similar to flowdef, except it has
#' jobids and previous jobids instead of only names.
#' This helps in creating the diagram for the flow.
#' Currently the only way to create a diagram is from a flow object,
#' by calling the function plot_flow.
#'
#' In future plot_flow may be able to accept a flowdef only.
#' @examples \dontrun{
#' create_jobs_mat(x = x)}
create_jobs_mat <- function(x){
	.Deprecated("to_flowdef")

	jobnames <- sapply(x@jobs, slot, "name")
	prev_jobs <- sapply(x@jobs, slot, "previous_job")
	prev_jobs <- sapply(prev_jobs, function(x) ifelse(length(x) > 0, paste(x,collapse=","), NA))
	dep_type <- sapply(x@jobs, slot, "dependency_type")
	sub_type <- sapply(x@jobs, slot, "submission_type")
	cpu <- sapply(x@jobs, slot, "cpu")
	nodes <- sapply(x@jobs, slot, "nodes")
	dat <- cbind(jobname = jobnames, prev_jobs, dep_type, sub_type, cpu_reserved = cpu, nodes)
	dat <- as.data.frame(dat, stringsAsFactors=FALSE)
	## ----------- handle cases where we have multiple dependencies
	rows <- grep(",",dat$prev_jobs)
	if(length(rows)>0){
		dat2 <- data.frame()
		for(row in rows){
			prev_jobs=strsplit(as.c(dat[row,]$prev_jobs),",")[[1]]
			dat2 <- rbind(dat2,cbind(jobname=dat[row,"jobname"], prev_jobs=prev_jobs,
															 dep_type=dat[row,"dep_type"],sub_type=dat[row,"sub_type"],
															 cpu_reserved=dat[row, "cpu_reserved"],
															 nodes=dat[row,"nodes"]))
		}
		dat <- rbind(dat[-rows,],dat2)
	}
	for(j in 1:4){
		jobnames=unique(as.c(dat$jobname))
		jobid <- 1:length(jobnames);names(jobid)=jobnames
		prev_jobid <- jobid[as.c(dat$prev_jobs)]
		dat$jobid <- jobid[as.c(dat$jobname)];dat$prev_jobid <- prev_jobid
		dat <- dat[order(dat$prev_jobid, dat$jobid, na.last=FALSE, decreasing=FALSE),]
	}
	return(dat)
}

## --- remove this in the next version change
.create_jobs_mat  <- function(...){
	.Deprecated("create_jobs_mat")
	create_jobs_mat(...)
}

# nocov end
