

# Extract jobids from HPCC submission output
# 
# Internal function (used by \link{submit_flow}), to extract jobids from hpcc submission output.
# 
# @param jobids output from HPCC upon job submission, as a character vector
# @param platform string specifying the platform. This determines how the jobids are parsed
#' @importFrom stats na.omit
parse_jobids <- function(jobids, platform){

	## --- TORQUE
	##     Example
	##  40947.dqsfacpriv01.mdanderson.edu
	##  parse into: -->>> 40947
	## if there is a . in the middle, get stuff before it
	if(platform=="torque")
		jobids <- gsub(opts_flow$get("flow_parse_torque"),"\\1", jobids)

	## --- LSF
	## --- Example:
	## Job <4809> is submitted to queue <transfer>.
	##  parse into: --->>> 4809
	if(platform=="lsf")
		jobids <- gsub(opts_flow$get("flow_parse_lsf"),"\\1", jobids)

	## --- moab
	## --- Example (has empty lines):
	## parse into: --->>> 97724
	if(platform == "moab")
		jobids = gsub(opts_flow$get("flow_parse_moab"), "\\1", jobids)

	if(platform == "sge")
		jobids = gsub(opts_flow$get("flow_parse_sge"), "\\1", jobids)

	if(platform == "slurm")
		jobids = gsub(opts_flow$get("flow_parse_slurm"), "\\1", jobids)

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



