
#' @rdname check
#' @title Check consistency of flowdef and flowmat
#' @description check consistency of objects
#' Currently checks objects S3 flowdef, flowmat
#'
#' @param x a flowdef or flowmat object
#' @param ... suppled to \code{check.classname} function
#'
#' @export
check <- function(x, ...) {
	UseMethod("check")
}



#' @rdname check
#' @export
check.flowmat <- function(x, ...){
	numcol = ncol(x)
	if (numcol < 3)
		stop("We need at lease 3 columns")

	return(x)

}


#' @rdname check
#' @export
check.flowdef <- function(x, ...){

	dep_types = c("none", "serial", "gather", "burst")
	sub_types = c("serial", "scatter")
	need_cols = c("jobname", "prev_jobs", "sub_type", "dep_type")
	opt_cols = c("platform", "cpu_reserved", 'walltime', 'queue')

	if (any(!need_cols  %in% colnames(x)))
		stop(error("def.need.cols"), paste(need_cols, collapse = " "))

	if (any(!need_cols  %in% colnames(x)))
		stop(error("def.opt.cols"), paste(opt_cols, collapse = " "))

	if (sum(!x$dep_type %in% dep_types))
		stop("Dependency type not recognized.\n Inputs are: ",
				 paste(x$dep_type, collapse = " "),
				 ".\nAnd they can be one of ", paste(dep_types, collapse = " "))
	if (sum(!x$sub_type %in% sub_types))
		stop("Submission type not recognized. Inputs are: ", paste(x$sub_type, collapse = " "),
				 ".\nAnd they can be one of  ", paste(sub_types, collapse = " "))

	## check if some jobs are put as dependencies but not properly defined
	## prevjob_exists()
	x$prev_jobs = gsub("\\.|NA", "none", x$prev_jobs)
	x$prev_jobs = ifelse(x$prev_jobs=="", "none", x$prev_jobs)
	x$prev_jobs = ifelse(is.na(x$prev_jobs), "none", x$prev_jobs)

	prev_jobs = unlist(strsplit(x$prev_jobs[!(x$prev_jobs == "none")], ","))

	miss_jobs = prev_jobs[!prev_jobs %in% x$jobname]
	if (length(miss_jobs) > 0){
		print(kable(x))
		stop("Some jobs do not exist, but are present in prev_jobs: ", miss_jobs, "\n")
	}
	## check if dep is none, but prev jobs defined

	## --- check if there are rows where prev_job specied but dep NOT specified
	extra_rows = (x$dep_type == "none" & x$prev_jobs != "none")
	if (sum(extra_rows) > 0){
		print(kable(x[extra_rows,]))
		stop(error("prev_job.wo.dep_type"))
	}

	extra_rows = (x$dep_type != "none" & x$prev_jobs == "none")
	if (sum(extra_rows)){
		print(kable(x[extra_rows,]))
		stop(error("dep_type.wo.prev_job"))
	}

	## check resource requirements, substitute by defaults

	if (FALSE){ # does not work as expected
		## --- if flow def has memory and not memory_reserved, handle it
		if (is.null(x$memory_reserved) & !is.null(x$memory)){
			x$memory_reserved = x$memory
		}
		if (is.null(x$cpu_reserved) & !is.null(x$cpu)){
			x$cpu_reserved = as.numeric(x$cpu)
		}
	}

	x$cpu_reserved = as.numeric(x$cpu_reserved)
	#print(x)
	## check all previous jobs defined in names
	## code previous jobs as NA
	## allowable types:
	## previous job
	##      scatter --(serial)--> scatter
	##      scatter --(serial)--> scatter
	##      scatter --(gather)--> scatter
	##      scatter --(gather)--> serial
	##      serial  --(serial)--> scatter
	##      serial  --(burst)--> scatter
	## not allowed:
	##      any --(none)--> any
	invisible(x)
}
