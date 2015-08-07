# nocov start



## depreciated

.run_sleep <- function(platform, ...){
	message("\n\nLets work on a simple example")
	exdata = file.path(system.file(package = "flowr"), "extdata")
	flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"), id_column = "samplename")
	## this has a bunch of samples, so let us subset one of them
	flow_mat = subset(flow_mat, flow_mat$samplename == "sample1")
	flowdef = read_sheet(file.path(exdata, "example1_flow_def.txt"), id_column = "jobname")
	flowdef = to_flowdef(flowdef) ## check for consistency
	fobj <- to_flow(x = flow_mat, def = flowdef,
									flowname = "ex_sleep", platform = platform, submit = FALSE, ...)
	invisible(fobj)
}

# nocov end
