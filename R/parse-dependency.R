#' @export
parse_dependency <- function(x, ...) {
	# 	message("input x is ", class(x))
	UseMethod("parse_dependency")
}


parse_dependency.local <- function(...){
	return("")
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
	dep_type = x@dependency_type
	if(dep_type == 'gather'){
		dep = sprintf("-l depend=afterok:%s", 
									paste(unlist(x@dependency), collapse = ":"))
	}else if(dep_type == "serial"){
		dep <- sprintf("-l %s", paste(" depend=afterok:",
																	x@dependency[[index]], 
																	sep="", collapse=":"))
	}else if(dep_type == "burst"){
		index=1
		dep <- sprintf("-l %s",paste(" depend=afterok:", 
																 x@dependency[[index]], sep="",
																 collapse=":"))
	}else{dep = ""}
	return(dep)
}

parse_dependency.sge <- function(x, index, ...){
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


## this has not been tested !
parse_dependency.slurm <- function(x, index, ...){
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




