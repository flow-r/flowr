
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
	if(numcol < 3)
		stop("We need at lease 3 columns")

	return(x)

}


#' @rdname check
#' @export
check.flowdef <- function(x, ...){

	dep_types = c("none", "serial", "gather", "burst")
	sub_types = c("serial", "scatter")

	if(sum(!x$dep_type %in% dep_types))
		stop("Dependency type not recognized.\n Inputs are: ",
				 paste(x$dep_type, collapse = " "),
				 ".\nAnd they can be one of ", paste(dep_types, collapse = " "))
	if(sum(!x$sub_type %in% sub_types))
		stop("Submission type not recognized. Inputs are: ", paste(x$sub_type, collapse = " "),
				 ".\nAnd they can be one of  ", paste(sub_types, collapse = " "))
	## check if some jobs are put as dependencies but not properly defined
	x$prev_jobs = gsub("\\.|none", NA, x$prev_jobs)
	prev_jobs = unlist(strsplit(x$prev_jobs[!is.na(x$prev_jobs)], ","))
	miss_jobs = prev_jobs[!prev_jobs %in% x$jobname]
	if(length(miss_jobs) > 0)
		stop("Some jobs do not exist: ", miss_jobs, "\n", kable(x))
	## check if dep is none, but prev jobs defined
	x$prev_jobs = ifelse(x$prev_jobs=="", NA, x$prev_jobs)
	rows = x$dep_type == "none" & !is.na(x$prev_jobs)
	if(sum(rows) > 0){
		print(kable(x[rows,]))
		stop("\nA Jobname has been specified as a previous job,",
				 "but dependency type is not clearly specified.",
				 "Either change the dependency type into: serial, gather, burst.\n",
				 "Or remove this dependency.")
	}
	rows = x$dep_type != "none" & is.na(x$prev_jobs)
	if(sum(rows)){
		print(kable(x[rows,]))
		stop("Previous jobs NOT defined, but dependency type is NOT none")
	}

	## check resource requirements, substitute by defaults

	if(FALSE){ # does not work as expected
		## --- if flow def has memory and not memory_reserved, handle it
		if(is.null(x$memory_reserved) & !is.null(x$memory)){
			x$memory_reserved = x$memory
		}
		if(is.null(x$cpu_reserved) & !is.null(x$cpu)){
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
