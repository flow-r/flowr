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
					"\nDetection of failure point is currently not supported")
	
	if(x == "perm.flow_details.file")
		y = c("flow_details.txt is not writable. Will skip updating it.")
	
	if(x == "perm.flow_status.file")
		y = c("flow_status.txt is not writable. Will skip updating it.")
	
	
  return(y)
}