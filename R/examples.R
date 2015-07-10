

.run_sleep <- function(platform, ...){
	message("\n\nLets work on a simple example")
	exdata = file.path(system.file(package = "flowr"), "extdata")
	flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"), id_column = "samplename")
	## this has a bunch of samples, so let us subset one of them
	flow_mat = subset(flow_mat, flow_mat$samplename == "sample1")
	flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"), id_column = "jobname")
	fobj <- to_flow(x = flow_mat, def = flow_def,
									flowname = "ex_sleep", platform = platform, submit = FALSE, ...)
	invisible(fobj)
}

#' run pipelines
#' Running examples flows
#' This wraps a few steps:
#' Get all the commands to run (flow_mat)
#' Create a `flow` object, using flow_mat and a default flow_def (picked from the same folder).
#' Use `submit_flow()` to submit this to the cluster.
#' 
#' @param x name of the pipeline to run. This is a function called to create a flow_mat.
#' @param flow_mat flow matrix, with commands to run (if we already have this, start here)
#' @param flow_def flow definition
#' @aliases run_flow
#' @export
run <- function(x="sleep", type = "example", platform, flow_mat, flow_def, execute = FALSE, ...){
	library(flowr)
	message("\n\nPerforming initial setup....")
	setup()
	message("Running example on platform:\t\t\t", platform)
	if(is.character(x))
		if(x == "sleep")
			fobj <- .run_sleep(platform = platform, ...)
	tmp <- submit_flow(fobj, execute = execute)
	return("Done !")
}

#' @export
run_flow = run

