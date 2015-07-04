
setClass("flow_def", contains = "data.frame") 
#http://www.carlboettiger.info/2013/09/11/extending-data-frame-class.html

#' @export
check <- function(x, ...) {
	UseMethod("check")
}

is.flow_def <- function(x){
	class(x) == "flow_def"
}

#' @export
#' @importFrom knitr kable
check.flow_def <- function(x, 
													 sub_types = c("serial", "scatter"),
													 dep_types = c("none", "serial", "gather", "burst")){
	if(sum(!x$dep_type %in% dep_types)) 
		stop("Dependency type not recognized ", paste(x$dep_type, collapse = " "), 
				 "should be from ", paste(dep_types, collapse = " "))
	if(sum(!x$sub_type %in% sub_types)) 
		stop("Submission type not recognized ", paste(x$sub_type, collapse = " "), 
				 " should be from ", paste(sub_types, collapse = " "))
	## check if some jobs are put as dependencies but not properly defined
	x$prev_jobs = gsub("\\.|none", NA, x$prev_jobs)
	prev_jobs = unlist(strsplit(x$prev_jobs[!is.na(x$prev_jobs)], ","))
	miss_jobs = prev_jobs[!prev_jobs %in% x$jobname]
	if(length(miss_jobs) > 0) 
		stop("Some jobs do not exist: ", miss_jobs, "\n", kable(x))
	## check if dep is none, but prev jobs defined
	x$prev_jobs = ifelse(x$prev_jobs=="", NA, x$prev_jobs)
	rows = x$dep_type == "none" & !is.na(x$prev_jobs)
	if(sum(rows)){
		print(kable(x[rows,]))
		stop("\nPrevious jobs defined, but dependency type is none")
	}
	rows = x$dep_type != "none" & is.na(x$prev_jobs)
	if(sum(rows)){
		print(kable(x[rows,]))
		stop("Previous jobs NOT defined, but dependency type is NOT none")
	}
	
	## -- convert to numeric
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

#

#' @export
as.flow_def <- function(x){
	if(is.flow_def(x))
		return(x)
	## ---- assuming x is a file
	if(is.data.frame(x))
		y <- new("flow_def", x)
	if(is.character(x)){
		message("Def seems to be a file, reading it...")
		y <- new("flow_def", read_sheet(x, id_column = "jobname"))
	}
	y = check(y)
	return(y)
}

#' Create a skeleton flow definition
#' @param jobnames
#' @param fl path to a matrix with commands to run
#' @details flow_tab: as defined by fl is a (minimum) three column matrix with
#' samplename, jobname, cmd
#' @export
sample_flow_def <- function(fl, jobnames, cols){
	if(!missing(fl)){
		mat <- read_sheet(fl)
		jobnames <- unique(mat$jobname)
		message("Following jobnames detected: ", 
						paste(jobnames, collapse = " "))
	}
	njobs = length(jobnames)
	df <- data.frame(jobname = jobnames,
									 prev_jobs = c("none", jobnames[-njobs]),
									 dep_type = c("none", rep("serial", njobs - 1)),
									 sub_type = rep('scatter', njobs),
									 queue = rep("medium", njobs),
									 memory_reserved = rep("163185", njobs),
									 walltime = rep("23:00", njobs),
									 cpu_reserved = rep(1, njobs), stringsAsFactors = FALSE)
	message("Creating a skeleton flow_def")
	if(!missing(fl))
		write.table(df, file = file.path(dirname(fl), "flow_def_ex.txt"), 
								sep = "\t", row.names = FALSE, quote = FALSE)
	invisible(df)
}




