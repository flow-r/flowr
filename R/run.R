

#' run pipelines
#'
#' @description
#' Running examples flows
#' This wraps a few steps:
#' Get all the commands to run (flow_mat)
#' Create a `flow` object, using flow_mat and a default flowdef (picked from the same folder).
#' Use `submit_flow()` to submit this to the cluster.
#'
#' @param x name of the pipeline to run. This is a function called to create a flow_mat.
#' @param def flow definition
#' @param flow_run_path passed onto to_flow. Default it picked up from flowr.conf. Typically this is ~/flowr/runs
#' @param platform what platform to use, overrides flowdef
#' @param execute TRUE/FALSE
#' @param ... passed onto the pipeline function specified in x
#'
#' @export
#'
#' @importFrom params load_opts read_sheet write_sheet
#'
#' @aliases run_flow
run <- function(x,
	platform,
	def,
	flow_run_path = get_opts("flow_run_path"),
	execute = FALSE,  ...){

	#print(get_opts("flow_run_path"))
	## find a Rscript with name {{x}}.R

	message("\n##--- fetching pipeline... ")
	pip = fetch_pipes(x, last_only = TRUE)

	if(missing(x))
		stop("Please choose a pipeline to run, from the above list.")


	## --- source the file and get the main function from it
	source(pip$pipe, TRUE)
	func = get(x) ## find function of the original name


	message("\n##--- loading confs....")
	## load default options for the pipeline
	confs = c(fetch_conf("flowr.conf"),
		fetch_conf("ngsflows.conf"),
		pip$conf)
	print(kable(as.data.frame(confs)))
	load_opts(confs, verbose = FALSE, check = FALSE)

	message("\n##--- creating flowmat....")
	## crate a flowmat
	args <- list(...)
	out = do.call(func, args)


	message("\n##--- stitching a flow object....")
	## get a flowdef
	if(missing(def))
		def = as.flowdef(pip$def)
	## create a flow object
	fobj = to_flow(x = out$flowmat,
		def = def,
		platform = platform,
		flowname = x,
		flow_run_path = flow_run_path)

	## submit the flow
	message("\n##--- submitting....")
	fobj = submit_flow(fobj, execute = execute)

	invisible(fobj)
}


#' @rdname run
#' @export
run_pipe <- run


if(FALSE){

	debug(run_pipe)
	run_pipe("sleep_pipe", samplename = "samp2")

}

## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##

.run <- function(x = "sleep", type = "example", platform, flowmat, def, execute = FALSE, ...){
	.Deprecated("run")
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






