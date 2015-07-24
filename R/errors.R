#' @title Error Handler
#' @description function to handle all error descriptions
#' @keywords internal
#' @param x this is a string(s) with short error summary
error <- function(x){

	## errors in rerun/kill/status
	## basically all functions which work after submission
	if("no.flow_details.file" %in% x){
    y = c("Looks like flow_details.txt does not exists in flow path.",
    "\nThis could happen due to multiple reasons.",
    "\nOne of them being flow was not completely submitted.",
    "\nFlow details file is created at the end of submission of all jobs")
	}

	if(x == "no.start_from")
		y = c("Please mention where to start this flow from.",
					"\nDetection of failure point is currently not supported",
					"Use start_from=<jobname>")

	if(x == "perm.flow_details.file")
		y = c("flow_details.txt is not writable. Will skip updating it.")

	if(x == "perm.flow_status.file")
		y = c("flow_status.txt is not writable. Will skip updating it.")

	if(x == "no.shell")
		y = "There are no shell scripts, did this flow run ?"

	if(x == "jobid.non.num")
		y = "Unable to parse JOB IDs as numbers, it likely submission failed. OR parsing of job ids failed \n\n"

	if(x == "no.conf")
		y = "Configuration file does not exist, loading skipped. Expecting a file at: "

	if(x == "no.def")
		y = "Flow definition file does not seems to exist. Expecting a file at: "

	if(x == "no.pipe")
		y = "Could not find a pipeline by the name of, "

	if(x == "def.need.cols")
		y = "flowdef needs these columns to proceed: "

	if(x == "def.opt.cols")
		y = "flowr works better with these columns in flowdef: "

	if(x == "prev_job.wo.dep_type")
		y = c("There are rows in flowdef where a dependency type is not specified,",
			"but dependency type is specified",
			"This is incompatible, either specify both or none.",
			"Possible values for dependency_type are: serial, gather, burst.")

	if(x == "dep_type.wo.prev_job")
		y = c("There are rows in flowdef where a previous job is not specified,",
			"but dependency type is specified",
			"This is incompatible, either specify both or none.")


	return(y)
}
