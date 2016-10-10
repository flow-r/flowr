

## Editing any function in this file needs advanced testing
## proceed with caution !


# render dependency
# Advanced use, for debugging. Or adding a currenltly un-supported platform.
# @param x is a `job` object
# @param ... not used
render_dependency <- function(x, ...) {
	# 	message("input x is ", class(x))
	UseMethod("render_dependency")
}


render_dependency.local <- function(...){
	return("")
}

## http://docs.adaptivecomputing.com/torque/4-1-4/Content/topics/commands/qsub.htm
render_dependency.torque <- function(x, index, ...){
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


## ti kills orphan jobs
render_dependency.lsf <- function(x, index, ...){
	#message(index)
	dep_type = x@dependency_type
	if(dep_type == 'gather'){
		dep <- sprintf("-w '%s' -ti",
									 paste(unlist(x@dependency), collapse = " && "))
	}else if(dep_type == "serial"){
		dep <- sprintf("-w '%s' -ti", paste(x@dependency[[index]],
																		collapse=" && "))
	}else if(dep_type == "burst"){
		index=1
		dep <- sprintf("-w '%s' -ti", paste(x@dependency[[index]],
																		collapse=" && "))
	}else{dep = ""}
	return(dep)
}

render_dependency.moab <- function(x, index, ...){
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

render_dependency.sge <- function(x, index, ...){
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
render_dependency.slurm <- function(x, index, ...){
	dep_type = x@dependency_type
	if(dep_type == 'gather'){
		dep = sprintf("--dependency==afterok:%s",
									paste(unlist(x@dependency), collapse = ":"))
	}else if(dep_type == "serial"){ ## collapse jobs at a specific index
		dep <- sprintf("--dependency=afterok:%s", 
									 paste(x@dependency[[index]], sep="", collapse=":"))
	}else if(dep_type == "burst"){
		index=1 ## ALL of them would see index 1
		dep <- sprintf("--dependency=afterok:%s",
									 paste(x@dependency[[index]], sep="", collapse=":"))
	}else{dep = ""}
	return(dep)
}




