### A class that contains bam file information
### Copyright 2014, Sahil Seth, all rights reserved
### sahil.seth@me.com
### A few functions to supplement those already in this package.
#### -----------------------

#### ----------------------- Declaring new classes
#### ----------------------- This class is a link between cluster and job management

#' @title queue defines the class
#' @exportClass queue
setClass("queue", representation(submit_exe = "character", ## submit job
                                 queue = "character", ## type of queue
                                 jobname = "character", ## name of a job, in the batch queue
                                 nodes = "numeric", ## number of nodes
                                 cpu = "numeric",   ## number of cpus
                                 memory = "character", ## memory to be reserved
                                 dependency = "list", ## job id
                                 walltime = "character", ## walltime
                                 cwd = "character", ## home
                                 stderr = "character", ## stderr
                                 stdout = "character",
                                 email = "character", ## email
                                 type = "character",  ## torque etc
                                 format = "character", ## cmd format
                                 extra_opts= "character", ## extra options for your queue
                                 server = "character")) ## address of head node

setClass("local", contains = "queue")
setClass("torque", contains = "queue")
setClass("pbs", contains = "queue")
setClass("lsf", contains = "queue")
setClass("sge", contains = "queue")

#### ----------------------- represents a single job
#' job defines the class
#' @exportClass job
setClass("job", representation(cmds = "character",
                               name = "character",## for creating stdout etc
                               base_path = "character",
                               id = "character", ## can be multiple
                               status = "character", ## status
                               submission_type = "character", ## scatter, serial
                               dependency_type = "character", ## gather, serial
                               previous_job = "character",
                               script = "character", ## the final script which has been used (if multiple cmds the last one)
                               next_job = "character"),
         contains = "queue") ## a string of cmd to run

#' flow defines the class
#' @exportClass flow
setClass("flow", representation(jobs = "list",
                                flow_base_path = "character",
                                flow_path = "character",
                                trigger_path = "character",
                                desc = "character",
                                status = "character", ## status
                                mode = "character", ## what kind of flow this is
                                name = "character"))


#### ---------------------- Functions to create new classes
#' Create a \code{queue} object which containg details about how a job is submitted.
#'
#' This function defines the queue used to submit jobs to the cluster. In essence details about the
#' computing cluster in use.
#'
#' @param object this is not used currenlty, ignore.
#' @param submit_exe the exact command used to submit jobs to the cluster example \code{qsub}
#' \code{bsub} etc.
#' @param queue the type of queue your group usually uses
#' @param nodes number of nodes you would like to request. \emph{optional} [Used by class job]
#' @param cpu number of cpus you would like to reserve [Used by class job]
#' @param dependency a list of jobs to complete before starting this one [Used by class job]
#' @param jobname name of this job in the computing cluster [Used by class job]
#' @param walltime max walltime of a job. [Used by class job]
#' @param cwd [Used by class job]
#' @param stderr [Used by class job]
#' @param stdout [Used by class job]
#' @param email [Used by class job]
#' @param extra_opts [Used by class job]
#' @param type Required and important. Currently supported values are 'lsf' and 'torque'. [Used by class job]
#' @param format We have a default format for the final command line string generated for 'lsf' and 'torque'.
#' This defined the exact (\code{bsub}/\code{qsub}) used to submit the job. One of the most important features required is:
#' dependencies. More on them here:
#' @param server This is not implemented currently. This would specify the head node of the computing cluster. At this time submission needs to be done on the head node.
#' @keywords queue
#' @export
#' @examples
#' queue(type='lsf')
queue <- function(object, submit_exe, queue="long", nodes=1, cpu=1,
                  dependency = list(), jobname = "name",
                  walltime = "72:00:00", cwd="~/flows", memory = "1",
                  stderr = "~/flows/tmp", stdout = "~/flows",
                  email = Sys.getenv("USER"),
                  type = "torque", format = "", extra_opts = "",
                  server = "localhost"){
    if(!missing(object)){
    }
    if(type=="torque"){
        format="${SUBMIT_EXE} -N ${JOBNAME} -q ${QUEUE} -l nodes=${NODES}:ppn=${CPU} -l walltime=${WALLTIME} -l mem=${MEMORY} -S /bin/bash -d ${CWD} -V -o ${STDOUT} -m ae -M ${EMAIL} ${EXTRA_OPTS} ${CMD} ${DEPENDENCY}"
        object <- new("torque", submit_exe="qsub", queue=queue,
                      nodes=nodes,cpu=cpu,jobname=jobname,
                      dependency=dependency,walltime=walltime,
                      cwd=cwd,#stderr=stderr,
                      memory=memory,
                      stdout=stdout,email = email,type=type,
                      format=format, extra_opts = extra_opts,
                      server=server)
    }else if(type=="lsf"){
        format="${SUBMIT_EXE} -q ${QUEUE} -J ${JOBNAME} -o ${STDOUT} -e ${STDERR} -n ${CPU} -cwd ${CWD} -m ${MEMORY} ${EXTRA_OPTS} ${DEPENDENCY} '<' ${CMD} "
        object <- new("lsf", submit_exe="bsub",queue=queue,
                      nodes=nodes,cpu=cpu,jobname=jobname,
                      dependency=dependency,walltime=walltime,
                      memory=memory,
                      cwd=cwd, stderr=stderr, stdout=stdout,email=email,type=type,
                      format=format, extra_opts = extra_opts,
                      server=server)
    }else if(type=="local"){
        object <- new("local", submit_exe="bash ",jobname=jobname)
    }else{
        object <- new("queue", submit_exe=submit_exe,queue=queue,
                      nodes=nodes,
                      cpu=cpu,dependency=dependency,walltime=walltime,
                      cwd=cwd,stderr=stderr,stdout=stdout,email=email,type=type, extra_opts = extra_opts,
                      jobname=jobname,format=format,server=server)
    }
    return(object)
}

## submission_type: this decides that the cmds to be submittion in which manner
## flow_type: if multi dependencies, wait for all or according to order
#' job class
#' @param cmds the commands to run
#' @param base_path base path
#' @param parent_flow parent flow
#' @param name name of the job
#' @param q_obj queue object
#' @param submission_type submission type
#' @param status status
#' @param dependency_type depedency type
#' @param cpu no of cpu's reserved
#' @param previous_job character vector of previous job. If this is the first job, one can leave this empty, NA, NULL or ''. In future this could specify multiple previous jobs.
#' @param ... other passed onto object creation
#' @export
#' @examples
#' q_obj <- queue(type="torque")
#' j_obj <- job(q_obj=q_obj,cmd="sleep 2",cpu=1)
job <- function(cmds = "", base_path = "", parent_flow = "", name = "myjob",
                q_obj = new("queue"), previous_job = '', cpu = 1,
                submission_type=c("scatter", "serial"), status="", script = "",
                dependency_type = c("none", "gather", "serial", "burst"), ...){
    ## convert to numeric if possible
    cpu <- as.numeric(cpu)
    ## replace some of the arguments
    if(!missing(q_obj)){ ## if queue is provided use that to replace the things
        #mget(names(formals()),sys.frame(sys.nframe()))
        args <- as.list(match.call(expand.dots=TRUE))
        args <- args[names(args) %in% slotNames(class(q_obj))]
        args <- lapply(args,eval, sys.frame(-1)) ## by getting the values from a frame above
        object <- do.call("replace_slots", args=c(object=q_obj,args))
    }else{
        q_obj <- new("queue")
    }
    submission_type <- match.arg(submission_type)
    dependency_type <- match.arg(dependency_type)
    object <- new("job",cmds = cmds, object, name = name, submission_type = submission_type,
                  previous_job = previous_job,
                  dependency_type = dependency_type,status=status,...)
    return(object)
}

#' Flow constructor
#' @param jobs \code{list} A list of jobs to be included in this flow
#' @param name \code{character} Name of the flow. Defaults to \code{'newname'}
#' Used in \link{submit_flow} to name the working directories.
#' @param desc \code{character} Description of the flow
#' This is used to name folders (when submitting jobs, see \link{submit_flow}).
#' It is good practice to avoid spaces and other special characters.
#' An underscore '_' seems like a good word separator.
#' Defaults to 'my_super_flow'. We usually use this to put sample names of the data.
#' @param mode \code{character} Mode of submission of the flow.
#' @param flow_base_path The base path of all the flows you would submit.
#' Defaults to \code{~/flows}. Best practice to ignore it.
#' @param trigger_path \code{character}
#' Defaults to \code{~/flows/trigger}. Best practice to ignore it.
#' @param flow_path \code{character}
#' @param status \code{character} Not used at this time
#' @export
flow <- function(jobs=list(new("job")), name="newflow", desc = "my_super_flow",
                 mode=c("scheduler","trigger","R"), flow_base_path="~/flows",
                 trigger_path="", flow_path="", status=""){
    mode <- match.arg(mode)
    jobnames <-  sapply(jobs, slot, "name")
    names(jobs) = jobnames
    object <- new("flow", jobs=jobs, mode = mode, name = name, flow_base_path=flow_base_path,
                  trigger_path=trigger_path, flow_path=flow_path, desc=desc, status=status)
    return(object)
}

if(FALSE){

    #q.obj <- queue(type="torque")
    q2 <- queue(object=q.obj,cpu=5)

    replace_slots(q.obj,cpu=4,name="newname")

    ## class(q)
    ## test_queue(q, verbose=TRUE)
    ## hpcc.command.format <- "#{CMD} | qsub -N #{NAME} -q #{QUEUE} -l #{NODES}:#{PPN} -l #{WALLTIME} -S /bin/bash -d #{HOME} -V -e #{STDERR} -o #{STDERR} -m ae -M #{EMAIL}"
#     source("~/Dropbox/public/github.flow/R/generic.R")
#     source("~/Dropbox/public/github.flow/R/class-def.R")
    debug(job)
    q_obj <- queue(type="torque")
    cpu_aln=1
    j_obj <- job(q_obj=q_obj,cmd="sleep 2",cpu=cpu_aln)

    j_obj@base_path <- "~/tmp/flows"
    #trace(create_queue_cmd, browser, signature="queue")
    #debug(slots_as_list)
    j_obj <- submit_job(j_obj, execute = TRUE, verbose = TRUE,
                        wd="~/tmp/flows/test_2481e475-31a0-41fc-8b01-cf01272abc3a")

    j.obj <- job(queue=q.obj,cmd="sleep 2")

    ## flow name: align_merge
    ## job use bowtie on two of them
    ## merge them using picard

    f.align <- (name="align_merge")
    align.cmds <- sprintf("echo 'aligning using bowtie';sleep %s",
                          round(runif(10)*10,2))


}

