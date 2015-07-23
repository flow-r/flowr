
setClass("flowdef", contains = "data.frame")
#http://www.carlboettiger.info/2013/09/11/extending-data-frame-class.html

#' check consistency of objects
#' Currently only checks for flowdef
#' @param x a flowdef object
#' @param ... suppled to \code{check.classname} function
#' @export
check <- function(x, ...) {
	UseMethod("check")
}

is.flowdef <- function(x){
	class(x)[1] == "flowdef"
}

#' @export
check.flowdef <- function(x,
	sub_types = c("serial", "scatter"),
	dep_types = c("none", "serial", "gather", "burst"), ...){
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

## needs two new functions:
## check resources
## check relationships

#
#' Reeading a flow definition file and checking it.
#' @param x can be a data.frame or a path for a flow definition file
#' @export
as.flowdef <- function(x){
	## ---- assuming x is a file
	if(is.data.frame(x))
		y = x
	if(is.character(x)){
		if(!file.exists(x))
			stop(error("no.def"), x)
		message("def seems to be a file, reading it...")
		y <- read_sheet(x, id_column = "jobname")
	}
	class(y) <- c("flowdef", "data.frame")
	y = check(y)
	return(y)
}



## -----------   this section deals with making a skeleton flowdef



#' Crate a skeleton flow definition using a flowmat !
#' @param x can a path to a flowmat, flowmat object.
#' @param sub_type
#' @param dep_type
#' @param queue
#' @param platform
#' @param memory_reserved
#' @param cpu_reserved
#' @param walltime
#' @return
#' Returns a flow object. If execute=TRUE, fobj is rich with information about where and how
#' the flow was executed. It would include details like jobids, path to exact scripts run etc.
#' To use kill_flow, to kill all the jobs one would need a rich flow object, with job ids present.
#'
#' @export
to_flowdef <- function(x, ...) {
	message("input x is ", class(x)[1])
	UseMethod("to_flowdef")
	warnings()
}




#' @rdname to_flowdef
#' @export
to_flowdef.flowmat <- function(x,
	sub_type = "serial",
	dep_type = "gather",
	queue = "short",
	platform = "torque",
	memory_reserved = "2000", ## in MB
	cpu_reserved = "1",
	walltime = "1:00"){

		message("Creating a skeleton flow definition")
		jobnames <- unique(x$jobname)
		message("Following jobnames detected: ",
			paste(jobnames, collapse = " "))

		njobs = length(jobnames)
		def <- data.frame(jobname = jobnames,
			sub_type = sub_type,
			prev_jobs = c("none", jobnames[-njobs]),
			dep_type = c("none", rep(dep_type, njobs - 1)),
			queue = queue,
			memory_reserved = memory_reserved,
			walltime = walltime,
			cpu_reserved = cpu_reserved,
			platform = platform,
			stringsAsFactors = FALSE)

		def = as.flowdef(def)
		return(def)
	}



to_flowdef.flow <- function(x){
	slts = c(jobname = "name",
		prev_jobs = 'previous_job',
		dep_type = "dependency_type",
		sub_type = "submission_type",
		queue = "queue",
		memory_reserved = "memory",
		walltime = "walltime",
		nodes = "nodes",
		cpu_reserved = "cpu",
		status = "status",
		platform = "platform")
	tmp <- lapply(x@jobs, function(y){
		unlist(slots_as_list(y)[slts])
	})
	def = data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
	colnames(def) = names(slts)
	print(kable(def))
	def = as.flowdef(def)
	return(def)
}

#' Create a skeleton flow definition
#' A helper function to create a skeleton flow definition.
#'
#' @param jobnames names of the jobs in a flow
#' @param fl path to a matrix with commands to run
#' @details flow_tab: as defined by fl is a (minimum) three column matrix with
#' samplename, jobname, cmd
#' @export
to_flowdef.character <- function(x, jobnames){
	if(!missing(x)){
		mat <- read_sheet(x)
		mat = to_flowmat(mat)
	}
	def = to_flowdef(mat)
	write.table(def, file = file.path(dirname(x), "flowdef.txt"),
		sep = "\t", row.names = FALSE, quote = FALSE)
	invisible(def)
}


#' split_multi_dep
#' Split rows with multiple dependencies
#' @param x this is a flow def
split_multi_dep <- function(x){
	dat = x
	## ----------- handle cases where we have multiple dependencies
	rows <- grep(",",dat$prev_jobs)
	if(length(rows)>0){
		dat2 <- data.frame()
		for(row in rows){
			prev_jobs=strsplit(as.c(dat[row,]$prev_jobs),",")[[1]]
			dat2 <- rbind(dat2,
				cbind(jobname=dat[row,"jobname"], prev_jobs=prev_jobs,
					dep_type=dat[row,"dep_type"],sub_type=dat[row,"sub_type"],
					cpu_reserved=dat[row, "cpu_reserved"],
					nodes=dat[row,"nodes"]))
		}
		dat <- rbind(dat[-rows,],dat2)
	}
}

## examples
if(FALSE){
	def = system.file('vignettes/ex_flow2.def', package = "ngsflows")

}


.create_jobs_mat <- function(x){
	jobnames <- sapply(x@jobs, slot, "name")
	prev_jobs <- sapply(x@jobs, slot, "previous_job")
	prev_jobs <- sapply(prev_jobs, function(x) ifelse(length(x) > 0, paste(x,collapse=","), NA))
	dep_type <- sapply(x@jobs, slot, "dependency_type")
	sub_type <- sapply(x@jobs, slot, "submission_type")
	cpu <- sapply(x@jobs, slot, "cpu")
	nodes <- sapply(x@jobs, slot, "nodes")
	dat <- cbind(jobname = jobnames, prev_jobs, dep_type, sub_type, cpu_reserved = cpu, nodes)
	dat <- as.data.frame(dat, stringsAsFactors=FALSE)

	## re-arrange them..
	for(j in 1:4){
		jobnames=unique(as.c(dat$jobname))
		jobid <- 1:length(jobnames);names(jobid)=jobnames
		prev_jobid <- jobid[as.c(dat$prev_jobs)]
		dat$jobid <- jobid[as.c(dat$jobname)];dat$prev_jobid <- prev_jobid
		dat <- dat[order(dat$prev_jobid, dat$jobid, na.last=FALSE, decreasing=FALSE),]
	}
	return(dat)
}





