#' @title Error Handler
#' @description function to handle all error descriptions
#' @keywords internal
#' @param x this is a string(s) with short error summary
error <- function(x, ...){

	assert_character(x, 1)

	if(class(x) != "character")
		return("")

	## errors in rerun/kill/status
	## basically all functions which work after submission
	if("no.flow_details.file" %in% x){
		y = c("Looks like flow_details.txt does not exists in flow path.",
					"\nThis could happen due to multiple reasons.",
					"\nOne of them being flow was not completely submitted.",
					"\nFlow details file is created at the end of submission of all jobs")

	}else if (x == "start_from: missing") {
		y = c("Please mention where to start this flow from.",
					"\nDetection of failure point is currently not supported",
					" Use start_from=<jobname>")

	}else	if (x == "perm.flow_details.file") {
		y = c("flow_details.txt is not writable. Will skip updating it.")

	}else	if(x == "perm.flow_status.file") {
		y = c("flow_status.txt is not writable. Will skip updating it.")

	}else	if(x == "no.shell") {
		y = c("There are no shell scripts, did this flow run? Or perhaps this is a parent folder?",
		" I need a flowr working directory, ", 
		"which has folders for each job and tmp folder and files like flow_details etc..")

	}else	if(x == "jobid.non.num") {
		y = "Unable to parse JOB IDs as numbers, it likely submission failed OR parsing of job ids failed \n\n"

	}else	if(x == "no.conf") {
		y = "Configuration file does not exist, loading skipped. Expecting a file at: "

	}else	if(x == "no.def") {
		y = "Flow definition file does not seems to exist. Expecting a file at: "

	}else	if(x == "no.pipe") {
		y = "Could not find a pipeline with this name: "

	}else	if(x == "flowdef: missing required columns") {
		y = "flowdef needs these columns to proceed: "

	}else	if(x == "flowdef: missing resource columns") {
		y = "flowr works better with these columns in flowdef: "

	}else	if(x == "prev_job.wo.dep_type") {
		y = c("There are rows in flowdef where a dependency type is not specified, ",
					"but dependency type is specified.",
					"\nThis is incompatible, either specify both or none. ",
					"Possible values for dependency_type are: serial, gather, burst.")

	}else	if(x == "dep_type.wo.prev_job") {
		y = c("There are rows in flowdef where a previous job is not specified, ",
					"but dependency type is specified. ",
					"This is incompatible, either specify both or none.")

	}else {
		y = ""
	}

	return(c(x, "\n", y))
}
