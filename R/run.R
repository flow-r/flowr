#' run pipelines
#' Running examples flows
#' This wraps a few steps:
#' Get all the commands to run (flow_mat)
#' Create a `flow` object, using flow_mat and a default flowdef (picked from the same folder).
#' Use `submit_flow()` to submit this to the cluster.
#'
#' @param x name of the pipeline to run. This is a function called to create a flow_mat.
#' @param flow_mat flow matrix, with commands to run (if we already have this, start here)
#' @param flowdef flow definition
#' @param type adv.
#' @param platform what platform to use, overrides flowdef
#' @param execute TRUE/FALSE
#' @param ... passed onto the function used to create the flow_mat
#'
#'
#' @aliases run_flow
#' @export
run <- function(x="sleep", type = "example", platform, flowmat, def, execute = FALSE, ...){
	library(flowr)
	message("\n\nPerforming initial setup....")
	setup()
	message("Running example on platform:\t\t\t", platform)
	if(is.character(x))
		if(x == "sleep")
			fobj <- .run_sleep(platform = platform, ...)

	## x is the name of the function


	tmp <- submit_flow(fobj, execute = execute)
	return("Done !")
}

#' @export
run_pipe = run
run_flow = run


run_pipe <- function(x, type = "pipe", ...){
	func = search_pipe(x)
	args <- as.list(match.call(expand.dots=TRUE))
	## remove some of them
	args


}
