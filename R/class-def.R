### A class that contains bam file information
### Copyright 2014, Sahil Seth, all rights reserved
### sahil.seth@me.com
### A few functions to supplement those already in this package.
#### -----------------------

#### ----------------------- Declaring new classes
#### ----------------------- This class is a link between cluster and job management

#' @rdname queue
#' 
#' @exportClass queue
setClass("queue", representation(submit_exe = "character", ## submit job
																 queue = "character", ## type of queue
																 jobname = "character", ## name of a job, in the batch queue
																 nodes = "character", ## number of nodes, or name of the node
																 cpu = "numeric",   ## number of cpus
																 memory = "character", ## memory to be reserved
																 dependency = "list", ## job id
																 walltime = "character", ## walltime
																 cwd = "character", ## home
																 stderr = "character", ## stderr
																 stdout = "character",
																 email = "character", ## email
																 platform = "character",  ## torque etc
																 format = "character", ## cmd format
																 extra_opts= "character", ## extra options for your queue
																 server = "character")) ## address of head node


#### ----------------------- represents a single job
setClass("job", representation(cmds = "character",
															 name = "character",## for creating stdout etc
															 base_path = "character",
															 id = "character", ## can be multiple
															 uid = "character",
															 status = "character", ## status
															 exit_code = "numeric", ## status
															 submission_type = "character", ## scatter, serial
															 dependency_type = "character", ## gather, serial
															 previous_job = "character",
															 script = "character", ## the final script which has been used (if multiple cmds the last one)
															 trigger = "character",
															 next_job = "character"),
				 contains = "queue") ## a string of cmd to run

setClass("local", contains = "job")
setClass("torque", contains = "job")
setClass("pbs", contains = "job")
setClass("lsf", contains = "job")
setClass("sge", contains = "job")
setClass("moab", contains = "job")
setClass("slurm", contains = "job")


#' @rdname flow
#' 
#' @exportClass flow
setClass("flow", representation(jobs = "list",
																flow_run_path = "character",
																flow_path = "character",
																trigger_path = "character",
																desc = "character",
																status = "character", ## status
																mode = "character", ## what kind of flow this is
																name = "character",
																module_cmds = "character",
																version = "character",
																execute = "logical"))


#' @rdname queue
#' 
#' @title A \code{queue} object defines details regarding how a job is submitted
#'
#' @description 
#' Internal function (used by \link{to_flow}), to define the format used to submit a job.
#'
#' @param object this is not used currenlty, ignore.
#' @param platform Required and important. Currently supported values are 'lsf' and 'torque'. [Used by class job]
#' @param queue the type of queue your group usually uses
#' 'bsub' etc.
#' @param nodes [advanced use] number of nodes you would like to request. Or in case of torque name of the nodes.\emph{optional} [Used by class job]
#' @param cpu number of cpus you would like to reserve [Used by class job]
#' @param dependency [debug use] a list of jobs to complete before starting this one
#' @param walltime max walltime of a job.
#' @param email [advanced use] Defaults to system user, you may put you own email though may get tons of them.
#' @param extra_opts [advanced use] Extra options to be supplied while create the job submission string.
#' @param submit_exe [advanced use] Already defined by 'platform'. The exact command used to submit jobs to the cluster example 'qsub'
#' @param format [advanced use] We have a default format for the final command line string generated for 'lsf' and 'torque'.
#' @param verbose [logical] TRUE/FALSE
#' @param cwd [debug use] Ignore
#' @param jobname [debug use] name of this job in the computing cluster
#' @param stderr [debug use] Ignore
#' @param stdout [debug use] Ignore
#' @param server [not used] This is not implemented currently. This would specify the head node of the computing cluster. At this time submission needs to be done on the head node of the cluster where flow is to be submitted
#'
#'
#' @details
#' \strong{Resources}:
#' Can be defined **once** using a \link{queue} object and recylced to all the jobs in a flow. If resources (like memory, cpu, walltime, queue) are supplied at the
#' job level they overwrite the one supplied in \link{queue}
#' Nodes: can be supplied ot extend a job across multiple nodes. This is purely experimental and not supported.
#' 
#' \strong{Server}:
#' This a hook which may be implemented in future.
#' 
#' \strong{Submission script}
#' The 'platform' variable defines the format, and submit_exe; however these two are avaible for someone to create a custom submission command.
#' 
#' @inheritParams job
#' 
#' @keywords queue
#' 
#' @export 
#' @examples
#' qobj <- queue(platform='lsf')
queue <- function(object,
									platform = c('local', 'lsf', 'torque', 'sge', 'moab'),
									## --- format is a advanced option, use with caution
									format = "",
									## --- Following are replaced by job()
									queue = "long",
									walltime, memory, cpu = 1,
									## format
									extra_opts = "",
									submit_exe,
									nodes = '1',  ## only used in torque
									## debug use
									jobname = "name",
									email = Sys.getenv("USER"),
									dependency = list(),
									server = "localhost",
									verbose = FALSE,
									cwd = "",
									stderr = "",
									stdout = "",
									...){
	platform = match.arg(platform)
	if (!missing(object)){
		object = replace_slots(object = object, ...)
		return(object)
	}
	
	## --- setting defaults
	if (missing(walltime)){
		walltime = switch(platform,
											torque = "72:00:00",
											lsf = "72:00",
											"24:00")
		if (verbose)
			message("Setting default time to: ", walltime,
							". If this is more than queue max (/improper format), job will fail. You may change this in job()\n")
	}
	if (missing(memory)){
		memory = switch(platform,
										lsf = "10000",
										torque = "10g",
										"1000")
		if (verbose)
			message("Setting default memory to: ", memory,
							". If this is more than queue max (/improper format), job will fail.\n")
	}
	if (platform %in% c("torque", "sge")){
		
		if (missing(format))
			format="${SUBMIT_EXE} -N ${JOBNAME} -q ${QUEUE} -l nodes=${NODES}:ppn=${CPU} -l walltime=${WALLTIME} -l mem=${MEMORY} -S /bin/bash -d ${CWD} -V -o ${STDOUT} -m ae -M ${EMAIL} -j oe -r y -V ${EXTRA_OPTS} ${CMD} ${DEPENDENCY}"
		if(missing(submit_exe))
			submit_exe = "qsub"
		
		object <- new("torque", submit_exe=submit_exe, queue=queue,
									nodes=nodes,cpu=cpu,jobname=jobname,
									dependency=dependency,walltime=walltime,
									cwd=cwd, #stderr=stderr,
									memory=memory,
									stdout=stdout,email = email,platform=platform,
									#format=format,
									extra_opts = extra_opts,
									server=server)
	}else if (platform=="lsf"){
		## restrict cores to one node
		## bsub -q myqueue -J myjob -o myout -e myout -n cpu -cwd mywd -m mem -W 02:00 < script.sh
		## -r: rerun
		## -W: walltime
		## -M: max mem
		## -R rusage[mem=16385]: min mem (reserved mem)
		if (missing(format))
			format="${SUBMIT_EXE} -q ${QUEUE} -J ${JOBNAME} -o ${STDOUT} -e ${STDERR} -n ${CPU} -cwd ${CWD} -M ${MEMORY} -R rusage[mem=${MEMORY}] -R span[ptile=${CPU}] -W ${WALLTIME} -r ${EXTRA_OPTS} ${DEPENDENCY} '<' ${CMD} " ## rerun failed jobs
		if(missing(submit_exe))
			submit_exe = "bsub"
		
		object <- new("lsf", submit_exe=submit_exe,queue=queue,
									nodes=nodes, cpu=cpu, jobname=jobname,
									dependency=dependency, walltime=walltime,
									memory=memory,
									cwd=cwd, stderr=stderr,
									stdout=stdout, email=email,platform=platform,
									#format=format,
									extra_opts = extra_opts,
									server=server)
	}else if (platform=="local"){
		if(missing(submit_exe))
			submit_exe = "bash"
		
		object <- new("local", submit_exe=submit_exe,queue=queue,
									nodes=nodes, memory=memory,
									cpu=cpu,dependency=dependency,walltime=walltime,
									cwd=cwd,stderr=stderr,stdout=stdout,email=email,platform=platform, extra_opts = extra_opts,
									jobname=jobname,#format=format,
									server=server)
	}else if (platform %in% c("moab")){
		if (missing(format))
			format="${SUBMIT_EXE} -N ${JOBNAME} -l nodes=${NODES}:ppn=${CPU} -l walltime=${WALLTIME} -l mem=${MEMORY} -S /bin/bash -d ${CWD} -V -o ${STDOUT} -m ae -M ${EMAIL} -j oe -r y -V ${EXTRA_OPTS} ${CMD} ${DEPENDENCY}"
		if(missing(submit_exe))
			submit_exe = "msub"
		object <- new("moab", submit_exe=submit_exe, queue=queue,
									nodes=nodes,cpu=cpu,jobname=jobname,
									dependency=dependency,walltime=walltime,
									cwd=cwd,#stderr=stderr,
									memory=memory,
									stdout=stdout,email = email,platform=platform,
									extra_opts = extra_opts,
									server=server)
		
	}else{
		object <- new('queue', submit_exe=submit_exe,
									queue=queue,
									nodes=nodes, memory=memory,
									cpu=cpu,dependency=dependency,walltime=walltime,
									cwd=cwd,stderr=stderr,stdout=stdout,email=email,platform=platform, extra_opts = extra_opts,
									jobname=jobname,server=server)
	}
	return(object)
}

## submission_type: this decides that the cmds to be submittion in which manner
## flow_type: if multi dependencies, wait for all or according to order

#' Describing details of the job object
#' 
#' Internal function (used by to_flow), which aids in creating a job object.
#'
#' @param cmds the commands to run
#' @param name name of the job
#' @param q_obj queue object
#' @param submission_type submission type: A character with values: scatter, serial. Scatter means all the 'cmds' would be run in parallel as seperate jobs. Serial, they would combined into a single job and run one-by-one.
#' @param dependency_type depedency type. One of none, gather, serial, burst. If previous_job is specified, then this would not be 'none'. [Required]
#' @param cpu no of cpu's reserved
#' @param previous_job character vector of previous job. If this is the first job, one can leave this empty, NA, NULL, '.', or ''. In future this could specify multiple previous jobs.
#' @param memory The amount of memory reserved. Units depend on the platform used to process jobs
#' @param walltime The amount of time reserved for this job. Format is unique to a platform. Typically it looks like 12:00 (12 hours reserved, say in LSF), in Torque etc. we often see measuring in seconds: 12:00:00
#' @param ... other passed onto object creation. Example: memory, walltime, cpu
#'
#' @export
#' @examples
#' qobj <- queue(platform="torque")
#'
#' ## torque job with 1 CPU running command 'sleep 2'
#' jobj <- job(q_obj=qobj, cmd = "sleep 2", cpu=1)
#'
#' ## multiple commands
#' cmds = rep("sleep 5", 10)
#'
#' ## run the 10 commands in parallel
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
#'
#' ## run the 10 commands sequentially, but WAIT for the previous job to complete
#' jobj2 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial",
#'    dependency_type = "gather", previous_job = "job1")
#'
#' fobj <- flow(jobs = list(jobj1, jobj2))
#'
#' ## plot the flow
#' plot_flow(fobj)
#' \dontrun{
#' ## dry run, only create the structure without submitting jobs
#' submit_flow(fobj)
#'
#' ## execute the jobs: ONLY works on computing cluster, would fail otherwise
#' submit_flow(fobj, execute = TRUE)
#'
#' }
job <- function(cmds = "",
								name = "myjob",
								q_obj = new("queue"),
								previous_job = '',
								cpu = 1,
								memory, walltime,
								submission_type = c("scatter", "serial"),
								dependency_type = c("none", "gather", "serial", "burst"),
								...){
	#message(name)
	## convert to numeric if possible
	cpu <- as.numeric(cpu)
	## replace some of the arguments
	if (!missing(q_obj)){ ## if queue is provided use that to replace the things
		#mget(names(formals()),sys.frame(sys.nframe()))
		args <- as.list(match.call(expand.dots=TRUE))
		args <- args[names(args) %in% slotNames(class(q_obj))]
		args <- lapply(args,eval, sys.frame(-1)) ## by getting the values from a frame above
		object <- do.call("replace_slots", args=c(object=q_obj,args))
	}else{
		formals(queue)
		object <- new("queue", ...)
	}
	submission_type <- match.arg(submission_type)
	dependency_type <- match.arg(dependency_type)
	if (prevjob_exists(previous_job) & dependency_type == 'none')
		stop("Previous job specified, but you have not specified dependency_type")
	object <- new(q_obj@platform,
								cmds = cmds,
								object,
								name = name,
								submission_type = submission_type,
								previous_job = previous_job,
								status = "",
								dependency_type = dependency_type,...)
	return(object)
}

#' Describing the flow class
#' 
#' Internal function (used by \link{to_flow}), which aids in creating a flow object.
#' 
#' @param jobs \code{list}: A list of jobs to be included in this flow
#' @param name \code{character}: Name of the flow. ['newflow']
#' @param desc \code{character} Description of the flow, used to uniquely identify a 
#' flow instance. ['my_super_flow']
#' @param mode \code{character} Mode of submission of the flow (depreciated). ['scheduler']
#' @param flow_run_path The base path of all the flows you would submit. [~/flows]
#' @param trigger_path \code{character} [\code{~/flows/trigger}].
#' @param flow_path \code{character}: A unique path identifying a flow instance, populated by \link{submit_flow}.
#' @param status \code{character}: Status of the flow.
#' @param version version of flowr used to create and execute this flow.
#' @param execute executtion status of flow object. [FALSE]
#' @param module_cmds [advanced use] a character vector of cmds which will be pre-pended to all script of this pipeline. 
#' Could be cmds like \code{`module load mytool1;module load mytool2`}
#' @export
#' @examples
#' cmds = rep("sleep 5", 10)
#' qobj <- queue(platform='torque')
#' ## run the 10 commands in parallel
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
#'
#' ## run the 10 commands sequentially, but WAIT for the previous job to complete
#' ## Many-To-One
#' jobj2 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial",
#'  dependency_type = "gather", previous_job = "job1", name = "job2")
#'
#' ## As soon as first job on 'job1' is complete
#' ## One-To-One
#' jobj3 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter",
#'  dependency_type = "serial", previous_job = "job1", name = "job3")
#'
#' fobj <- flow(jobs = list(jobj1, jobj2, jobj3))
#'
#' ## plot the flow
#' plot_flow(fobj)
#' \dontrun{
#' ## dry run, only create the structure without submitting jobs
#' submit_flow(fobj)
#'
#' ## execute the jobs: ONLY works on computing cluster, would fail otherwise
#' submit_flow(fobj, execute = TRUE)
#' }
flow <- function(
	jobs = list(new("job")),
	name = "newflow",
	desc = "my_super_flow",
	mode = c("scheduler","trigger","R"),
	flow_run_path = opts_flow$get("flow_run_path"),
	trigger_path = "",
	flow_path = "",
	version = '0.0',
	status="created",
	module_cmds = opts_flow$get("module_cmds"),
	execute = ""){
	mode <- match.arg(mode)
	## create a list of jobs if nore already
	if (class(jobs) == "job") jobs = list(jobs)
	jobnames <-  sapply(jobs, slot, "name")
	names(jobs) = jobnames
	object <- new("flow", jobs=jobs,
								mode = mode,
								name = name, flow_run_path = flow_run_path,
								trigger_path = trigger_path,
								flow_path=flow_path,
								module_cmds = module_cmds,
								desc=desc,
								version = version,
								status=status)
	return(object)
}



if (FALSE){
	
	#q.obj <- queue(platform="torque")
	q2 <- queue(object=qobj,cpu=5)
	
	replace_slots(q.obj,cpu=4,name="newname")
	
	## class(q)
	## test_queue(q, verbose=TRUE)
	## hpcc.command.format <- "#{CMD} | qsub -N #{NAME} -q #{QUEUE} -l #{NODES}:#{PPN} -l #{WALLTIME} -S /bin/bash -d #{HOME} -V -e #{STDERR} -o #{STDERR} -m ae -M #{EMAIL}"
	#     source("~/Dropbox/public/github.flow/R/generic.R")
	#     source("~/Dropbox/public/github.flow/R/class-def.R")
	debug(job)
	q_obj <- queue(platform="torque")
	cpu_aln=1
	jobj <- job(q_obj=q_obj,cmd="sleep 2",cpu=cpu_aln)
	
	jobj@base_path <- "~/tmp/flows"
	#trace(create_queue_cmd, browser, signature="queue")
	#debug(slots_as_list)
	jobj <- submit_job(jobj, execute = TRUE, verbose = TRUE,
										 wd="~/tmp/flows/test_2481e475-31a0-41fc-8b01-cf01272abc3a")
	
	j.obj <- job(queue=q.obj,cmd="sleep 2")
	
	## flow name: align_merge
	## job use bowtie on two of them
	## merge them using picard
	
	f.align <- (name="align_merge")
	align.cmds <- sprintf("echo 'aligning using bowtie';sleep %s",
												round(runif (10)*10,2))
	
	
}

