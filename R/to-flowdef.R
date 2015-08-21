
#setClass("flowdef", contains = "data.frame")
#http://www.carlboettiger.info/2013/09/11/extending-data-frame-class.html

#' @rdname as.flowdef
#' @title flow definition
#' @description  Reeading a flow definition file and checking it.
#' @param x can be a data.frame or a path for a flow definition file
#' @param ... passed onto check.flowdef
#' @export
as.flowdef <- function(x, ...){
	## ---- assuming x is a file
	if(is.flowdef(x))
		return(check(x))
	if(is.data.frame(x))
		y = x
	if(is.character(x)){
		if(!file.exists(x))
			stop(error("no.def"), x)
		message("def seems to be a file, reading it...")
		y <- read_sheet(x, id_column = "jobname")
	}
	y$jobid <- 1:nrow(y)
	class(y) <- c("flowdef", "data.frame")
	y = check(y, ...)
	return(y)
}


#' @rdname as.flowdef
#' @export
is.flowdef <- function(x){
	class(x)[1] == "flowdef"
}


## needs two new functions:
## check resources
## check relationships



## -----------   this section deals with making a skeleton flowdef


#' @rdname to_flowdef
#' @title
#' Create a skeleton flow definition using a flowmat.
#'
#' @description Creation of a skeleton flow definition with several default values.
#'
#' All params may be of length one, or same as the number of jobnames
#'
#' @param x can a path to a flowmat, flomat or flow object.
#' @param sub_type submission type, one of: scatter, serial. Character, of length one or same as the number of jobnames
#' @param dep_type dependency type, one of: gather, serial or burst. Character, of length one or same as the number of jobnames
#' @param prev_jobs previous job name
#' @param queue Cluster queue to be used
#' @param platform platform of the cluster: lsf, sge, moab, torque, slurm etc.
#' @param memory_reserved amount of memory required.
#' @param cpu_reserved number of cpu's required
#' @param walltime amount of walltime required
#' @param ... not used
#'
#' @importFrom knitr kable
#' @export
to_flowdef <- function(x, ...){
	#message("input x is ", class(x)[1])
	UseMethod("to_flowdef")
	warnings()
}


guess_sub_dep <- function(x){

	lst <- lapply(1:nrow(x), function(i){
		cmds = "";
		prev_job = ""
		d_sub_type <- detect_sub_type(cmds = cmds)
		d_dep_type <- detect_dep_type(prev_job = prev_job, cmds = cmds)
		list(sub_type = d_sub_type, dep_type = d_dep_type)
	})
	return(lst)
}

#' @rdname to_flowdef
#' @export
to_flowdef.flowmat <- function(x,
															 sub_type,
															 dep_type,
															 prev_jobs,
															 queue = "short",
															 platform = "torque",
															 memory_reserved = "2000", ## in MB
															 cpu_reserved = "1",
															 walltime = "1:00", ...){

	message("Creating a skeleton flow definition")
	jobnames <- unique(x$jobname)
	message("Following jobnames detected: ",
					paste(jobnames, collapse = " "))

	njobs = length(jobnames)
	if(missing(dep_type))
		dep_type = c("none", rep("gather", njobs - 1))
	if(missing(sub_type))
		sub_type = "serial"
	if(missing(prev_jobs))
		prev_jobs = c("none", jobnames[-njobs])

	def <- data.frame(jobname = jobnames,
										sub_type = sub_type,
										prev_jobs = prev_jobs,
										dep_type = dep_type,
										queue = queue,
										memory_reserved = memory_reserved,
										walltime = walltime,
										cpu_reserved = cpu_reserved,
										platform = platform,
										stringsAsFactors = FALSE)

	def = as.flowdef(def)
	return(def)
}



#' @rdname to_flowdef
#' @export
to_flowdef.flow <- function(x, ...){
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
		y = slots_as_list(y)[slts]
		y$previous_job = paste(y$previous_job, collapse = ",")
		unlist(y)
	})
	def = data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
	colnames(def) = names(slts)
	#kable(def)
	def = as.flowdef(def)
	return(def)
}


#' @rdname to_flowdef
#' @description to_flowdef.character: x is a flowmat file.
#' @importFrom utils write.table
#' @export
to_flowdef.character <- function(x, ...){
	if(!missing(x)){
		mat <- read_sheet(x)
		mat = to_flowmat(mat)
	}
	def = to_flowdef(mat)
	write.table(def, file = file.path(dirname(x), "flowdef.txt"),
							sep = "\t", row.names = FALSE, quote = FALSE)
	invisible(def)
}



## examples
if(FALSE){
	def = system.file('vignettes/ex_flow2.def', package = "ngsflows")

}





