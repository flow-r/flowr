
#' @rdname check
#' @title Check consistency of flowdef and flowmat
#' @description check consistency of objects
#' Currently checks objects S3 flowdef, flowmat
#'
#' @param x a flowdef or flowmat object
#' @param verbose be chatty
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
#' @importFrom knitr kable
#' @export
check.flowdef <- function(x, verbose = get_opts("verbose"), ...){

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
	for(i in 1:nrow(x)){
		if(verbose) message("checking on: ", i," : ", x$jobname[i])
		check_dep_sub_type(dep = x$dep_type[i],
											 sub = x$sub_type[i],
											 p.sub = x$sub_type[i-1],
											 p.dep = x$sub_type[i-1],
											 verbose = verbose)
	}
	invisible(x)
}

##      scatter --(serial)--> scatter
##      scatter --(serial)--> scatter
##      scatter --(gather)--> scatter
##      scatter --(gather)--> serial
##      serial  --(serial)--> scatter
##      serial  --(burst)--> scatter
check_dep_sub_type <- function(dep, sub,
													p.dep, p.sub,
													verbose = get_opts("verbose")){
	v = verbose
	p.dep = ifelse(length(p.dep) == 0, "none", p.dep)
	p.sub = ifelse(length(p.sub) == 0, "none", p.sub)

	if(v) message("dep: ", dep, " sub: ", sub, " p.sub: ", p.sub, " p.dep: ", p.dep)

	## one to one
	if(dep == "serial" & sub == "serial"){
		if(v) message("rel: simple one:one")
	}else if(dep == "serial" & sub == "scatter" & p.sub == "scatter"){
		if(v) message("rel: complex one:one")

	## one to many
	}else if(p.sub == "serial" & sub == "scatter" & dep == "burst"){
		if(v) message("rel: one:many")
	}else if(p.sub == "serial" & sub == "scatter" & dep == "serial"){
		if(v) stop("rel: one:many. Please replace dep_type to burst")
	}else if(p.sub == "scatter" & sub == "scatter" & dep == "burst"){
		if(v) stop("rel: one:many. Please replace previous sub_type to serial")

	## many to one
	}else if(p.sub == "scatter" & sub == "serial" & dep == "gather"){
		if(v) message("rel: many:one")
	}else if(p.sub == "scatter" & sub == "scatter" & dep == "gather"){
		if(v) message("rel: many:one. Please change sub_type to serial")
	}else{
	}

}
