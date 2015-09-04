
#' @rdname check
#' @title Check consistency of flowdef and flowmat
#' @description 
#' Currently checks S3 flowdef & flowmat for consistency.
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
	opt_cols = c("platform", "cpu_reserved", 'walltime', 'queue', 'memory_reserved')

	if(verbose)
		message("\nchecking if required columns are present...")
	if (any(!need_cols  %in% colnames(x)))
		stop(c(error("flowdef: missing required columns"),
					 paste(need_cols, collapse = " ")))

	if(verbose)
		message("checking if resources columns are present...")
	if (any(!opt_cols  %in% colnames(x)))
		stop(c(error("flowdef: missing resource columns", msg = stop), 
							paste(opt_cols, collapse = ", ")))

	if(verbose)
		message("checking if dependency column has valid names...")
	if (sum(!x$dep_type %in% dep_types))
		stop("dep_type: invalid\n",
			"Dependency type not recognized.\n Inputs are: ",
				 paste(x$dep_type, collapse = " "),
				 ".\nAnd they can be one of ", paste(dep_types, collapse = ", "))

	if(verbose)
		message("checking if submission column has valid names...")
	if (sum(!x$sub_type %in% sub_types)){
		stop("sub_type: invalid\n",
				 "Submission type not recognized. Inputs are: ", paste(x$sub_type, collapse = " "),
				 ", and they can be one of: ", paste(sub_types, collapse = ", "))
	}
	
	## check if some jobs are put as dependencies but not properly defined
	## prevjob_exists()
	x$prev_jobs = gsub("\\.|NA", "none", x$prev_jobs)
	x$prev_jobs = ifelse(x$prev_jobs=="", "none", x$prev_jobs)
	x$prev_jobs = ifelse(is.na(x$prev_jobs), "none", x$prev_jobs)

	prev_jobs = unlist(strsplit(x$prev_jobs[!(x$prev_jobs == "none")], ","))

	if(verbose)
		message("checking for missing rows in def...")
	miss_jobs = prev_jobs[!prev_jobs %in% x$jobname]
	if (length(miss_jobs) > 0){
		message(paste(kable(x), collapse = "\n"))
		stop(c("extra jobs in prev_jobs\n",
					 "Some jobs do not exist, but are present in prev_jobs: ", miss_jobs, "\n"))
	}
	## check if dep is none, but prev jobs defined

	## --- check if there are rows where prev_job specied but dep NOT specified
	if(verbose)
		message("checking for extra rows in def...")
	extra_rows = (x$dep_type == "none" & x$prev_jobs != "none")
	if (sum(extra_rows) > 0){
		message(paste(kable(x[extra_rows,]), collapse = "\n"))
		stop(error("prev_job.wo.dep_type"))
	}

	extra_rows = (x$dep_type != "none" & x$prev_jobs == "none")
	if (sum(extra_rows)){
		message(paste(kable(x[extra_rows,]), collapse = "\n"))
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
	if(verbose)
		message("checking submission and dependency types...")
	if(verbose > 1) message("\tjobname\tprev.sub_type --> dep_type --> sub_type: relationship")
	for(i in 1:nrow(x)){
		if(verbose > 1) message("\t", i,": ", x$jobname[i], "\t", appendLF = FALSE)
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

	if(v > 1) 
		message(p.sub," --> ", dep, " --> ", sub, " ", appendLF = FALSE)

	## one to one
	if(dep == "serial" & sub == "serial"){
		if(v > 1) 
			message("rel: simple one:one")
	}else if(dep == "serial" & sub == "scatter" & p.sub == "scatter"){
		if(v > 1) 
			message("rel: complex one:one")

	## one to many
	}else if(p.sub == "serial" & sub == "scatter" & dep == "burst"){
		if(v > 1) 
			message("rel: one:many")
	}else if(p.sub == "serial" & sub == "scatter" & dep == "serial"){
		stop(c("detected relationship: one-to-many. ", 
					 "To define this, one must have dependency type as burst. ",
					 "Refer to docs.flowr.space for further details."))
	}else if(p.sub == "scatter" & sub == "scatter" & dep == "burst"){
		stop(c("\nDetected relationship: one-to-many. ", 
					 "To define this, one must have previous sub_type as serial. ",
					 "If the relationship is really one-to-one, you may want, ",
					 "scatter, serial and scatter as previous submission, dependency and submission types, respectively."))

	## many to one
	}else if(p.sub == "scatter" & sub == "serial" & dep == "gather"){
		if(v > 1) 
			message("rel: many:one")
	}else if(p.sub == "scatter" & sub == "scatter" & dep == "gather"){
		if(v > 1) 
			message("rel: many:one. Please change sub_type to serial")
	}else{
		if(v > 1) 
			message("")
	}

}
