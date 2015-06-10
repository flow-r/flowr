
#' @export
parse_jobids <- function(jobids, platform){
	## --- the string looks like: Job <4809> is submitted to queue <transfer>.
	if(platform=="lsf")
		jobids <- gsub(".*(\\<[0-9]*\\>).*","\\1", jobids)
	
	if(platform == "moab"){
		## --- output has multiple rows, split them
		jobids = na.omit(as.vector(jobids))
		## remove rows with missing data
		jobids = jobids[!jobids == ""]
	}
	
	## --- check how jobids looks
	chk = is.na(as.numeric(jobids))
	if(sum(chk) > 0)
		stop("Looks like jobsubmission failed. Please check the jobs submission format. Submission died with error: \n\n\n",
				 jobids)
	
	return(jobids)
}


#' @export
parse_dependency <- function(x, ...) {
# 	message("input x is ", class(x))
	UseMethod("parse_dependency")
}


parse_dependency.torque <- function(x, index, ...){
	dep_type = x@dependency_type
	if(dep_type == 'gather'){
		dep = sprintf("-W depend=afterok:%s", 
									paste(unlist(x@dependency), collapse = ":"))
	}else if(dep_type == "serial"){
		dep <- sprintf("-W %s", paste(" depend=afterok:",
							 																		 x@dependency[[index]], 
							 																		 sep="", collapse=":"))
	}else if(dep_type == "burst"){
		index=1
		dep <- sprintf("-W %s",paste(" depend=afterok:", 
																				x@dependency[[index]], sep="",
																				collapse=":"))
	}else{dep = ""}
	return(dep)
}

parse_dependency.lsf <- function(x, index, ...){
	#message(index)
	dep_type = x@dependency_type
	if(dep_type == 'gather'){
		dep <- sprintf("-w '%s'", 
													paste(unlist(x@dependency), collapse = " && "))
	}else if(dep_type == "serial"){
		dep <- sprintf("-w '%s'", paste(x@dependency[[index]], 
																					 collapse=" && "))
	}else if(dep_type == "burst"){
		index=1
		dep <- sprintf("-w '%s'", paste(x@dependency[[index]],
																					 collapse=" && "))
	}else{dep = ""}
	return(dep)
}

parse_dependency.moab <- function(x, index, ...){
	parse_dependency.torque(x, index, ...)
}
