#' parse_jobids
#' @export
parse_jobids <- function(jobids, platform){
	
	## --- TORQUE
	##     Example
	##  40947.dqsfacpriv01.mdanderson.edu 
	##  parse into: -->>> 40947
	## if there is a . in the middle, get stuff before it
	if(platform=="torque")
		jobids <- gsub(getOption("flow_parse_torque"),"\\1", jobids)

	## --- LSF
	## --- Example: 
	## Job <4809> is submitted to queue <transfer>.
	##  parse into: --->>> 4809
	if(platform=="lsf")
		jobids <- gsub(getOption("flow_parse_lsf"),"\\1", jobids)
	
	
	## --- moab
	## --- Example (has empty lines):
	## parse into: --->>> 97724
	if(platform == "moab")
		jobids = gsub(getOption("flow_parse_moab"), "\\1", jobids)
	
	if(platform == "sge")
		jobids = gsub(getOption("flow_parse_sge"), "\\1", jobids)

	if(platform == "slurm")
		jobids = gsub(getOption("flow_parse_slurm"), "\\1", jobids)
	
	## ""      "98337"
	## --- output has multiple rows, split them
	jobids = na.omit(as.vector(jobids))
	## remove rows with missing data
	jobids = jobids[!jobids == ""]
	
	## --- check how jobids looks
	##     forcing jobids to be numeric !!
	##     is this a big assumption ?
	chk = is.na(as.numeric(jobids))
	if(sum(chk) > 0)
		warning(error("jobid.non.num"))
	## stop(jobids)

	return(jobids)
}



