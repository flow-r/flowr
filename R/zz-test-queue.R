
#' @title test_queue
#' 
#' @description 
#' This function attempts to test the submission of a job to the queue.
#' We would first submit one single job, then submit another with a dependency to see if configuration works. 
#' This would create a folder in home called 'flows'.
#' 
#' [Depreciated]: This function has been superseded by \code{run("sleep_pipe", platform = "lsf", execute=TRUE)}
#' 
#' 
#' @param q_obj queue object
#' @param verbose toggle
#' @param ... These params are passed onto \code{queue}. \code{?queue}, for more information
#' @export
#' @examples
#' \dontrun{
#' test_queue(q_obj = q_obj, ... = ...)}
test_queue <- function(q_obj, verbose = TRUE, ...){
	if(missing(q_obj)){
		platform <- readline("What platform of cluster is this? Possible values: lsf, torque, sge\n>>> ");
		queue_prompt <- paste0("What queue does your group usually use?\n",
													 "For intitutions this could be",
													 "long, medium, normal, priority.\n",
													 "Not sure? Its best to consult your sysadmins.\n>>> ")
		queue <- readline(prompt = queue_prompt)
		extra <- readline(prompt = "extra options to the queue\n>>>")
		q_obj <- queue(platform = platform, queue = queue, extra_opts = extra, ...)
	}
	jobj1 <- job(cmds = 'sleep 60', q_obj = q_obj, name = 'job1')
	#cat("Submitted first job with script:", jobj1@script, "\n")
	jobj2 <- job(cmds = 'sleep 60', q_obj = q_obj, name = 'job2',
								previous_job = 'job1', dependency_type = "serial")
	#cat("Submitted second job with script:", jobj2@script, "\n")
	if(verbose) message("Creating a 'flow' of two jobs. Check 'flows' folder in your home for a",
											"new directory called test_....\n",
											"You may also do bjobs/qstat or a respective command for your",
											"scheduler to look at these jobs.\n\n\n")
	f_obj <- flow(jobs = list(jobj1, jobj2), desc = "test", flow_run_path = "~/flowr/runs")
	tmp <- submit_flow(f_obj, execute = TRUE, make_flow_plot = FALSE, verbose = TRUE)
	message("Flow path:\t", tmp@flow_path, "\n")
	message("First job ID: \t", tmp@jobs[[1]]@id, "\n")
	message("Second (dependent) job ID:\t", tmp@jobs[[2]]@id, "\n")
	message("Path to logs (1):\t", tmp@jobs[[1]]@stdout, "\n")
	message("Path to logs (2):\t", tmp@jobs[[2]]@stdout, "\n")
	## cmd.0 <- create_queue_cmd(jobj)
	## if(verbose) cat("An example command string looks like:\n", cmd.0)
	## cmd <- sprintf("echo 'sleep 1' | %s", cmd.0)
	## if( verbose ) print (cmd)
	## system(cmd)
	return(tmp)
}
#debug(test_queue)
