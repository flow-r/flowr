## A class that contains bam file information
## Copyright 2014, Sahil Seth, all rights reserved
## sahil.seth@me.com
## A few functions to supplement those already in this package.
#### -----------------------

#### ----------------------- Declaring new classes
#### ----------------------- This class is a link between cluster and job management
setClass("queue", representation(submit_exe = "character", ## submit job
                                 queue = "character", ## type of queue
                                 jobname = "character", ## name of a job, in the batch queue
                                 nodes = "numeric", ## number of nodes
                                 cpu = "numeric",   ## number of cpus
                                 dependency = "list", ## job id
                                 walltime = "character", ## walltime
                                 cwd = "character", ## home
                                 stderr = "character", ## stderr
                                 stdout = "character",
                                 email = "character", ## email
                                 type = "character",  ## torque etc
                                 format = "character", ## cmd format
                                 server = "character")) ## address of head node

setClass("local", contains = "queue")
setClass("torque", contains = "queue")
setClass("lsf", contains = "queue")
setClass("sge", contains = "queue")

#### ----------------------- represents a single job
setClass("job", representation(cmds = "character",
                               name = "character",## for creating stdout etc
                               base_path = "character",
                               id = "character", ## can be multiple
                               status = "character", ## status
                               submission_type = "character", ## scatter, serial
                               dependency_type = "character", ## gather, serial
                               previous_job = "character",
                               next_job = "character"),
         contains = "queue") ## a string of cmd to run

setClass("flow", representation(jobs = "list",
                                flow_base_path = "character",
                                flow_path = "character",
                                trigger_path = "character",
                                desc = "character",
                                status = "character", ## status
                                mode = "character", ## what kind of flow this is
                                name = "character"))


#### ---------------------- Functions to create new classes
queue <- function(object, submit_exe,queue="long",nodes=1,cpu=24,
                  dependency=list(),jobname="name",
                  walltime="72:00:00",cwd="~/flows",
                  stderr="~/flows/tmp",stdout="~/flows",email=Sys.getenv("USER"),
                  type="torque",format="",
                  server="localhost"){
    if(!missing(object)){
    }
    if(type=="torque"){
        format="${SUBMIT_EXE} -N ${JOBNAME} -q ${QUEUE} -l nodes=${NODES}:ppn=${CPU} -l walltime=${WALLTIME} -S /bin/bash -d ${CWD} -V -e ${STDERR} -o ${STDOUT} -m ae -M ${EMAIL} ${CMD} ${DEPENDENCY}"
        object <- new("torque", submit_exe="qsub",queue=queue,
                      nodes=nodes,cpu=cpu,jobname=jobname,
                      dependency=dependency,walltime=walltime,
                      cwd=cwd,stderr=stderr,stdout=stdout,email=email,type=type,
                      format=format,
                      server=server)
    }else if(type=="lsf"){
        format="bsub -q ${QUEUE} -J ${JOBNAME} -o ${STDOUT} -e ${STDERR} -n ${CPU} -cwd ${CWD} ${DEPENDENCY} '<' ${CMD} "
        object <- new("lsf", submit_exe="bsub",queue=queue,
                          nodes=nodes,cpu=cpu,jobname=jobname,
                      dependency=dependency,walltime=walltime,
                      cwd=cwd,stderr=stderr,stdout=stdout,email=email,type=type,
                      format=format,
                      server=server)
    }else if(type=="local"){
        object <- new("local", submit_exe="bash ",jobname=jobname)
    }else{
        object <- new("queue", submit_exe=submit_exe,queue=queue,
                      nodes=nodes,
                      cpu=cpu,dependency=dependency,walltime=walltime,
                      cwd=cwd,stderr=stderr,stdout=stdout,email=email,type=type,
                      jobname=jobname,format=format,server=server)
    }
    return(object)
}

## submission_type: this decides that the cmds to be submittion in which manner
## flow_type: if multi dependencies, wait for all or according to order
job <- function(cmds = "", base_path = "", parent_flow = "", name = "myjob",
                q_obj = new("queue"),
                submission_type=c("scatter", "serial"),status="",
                dependency_type = c("none", "gather", "serial", "burst"), ...){
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
                  dependency_type = dependency_type,status=status,...)
    return(object)
}


flow <- function(jobs=list(new("job")), name="newflow", desc,
                 mode=c("scheduler","trigger","R"), flow_base_path="~/flows",
                 trigger_path="", flow_path="", status=""){
    mode <- match.arg(mode)
    jobnames <-  sapply(jobs, slot, "name")
    names(jobs) = jobnames
    object <<- new("flow", jobs=jobs, mode = mode, name = name, flow_base_path=flow_base_path,
                  trigger_path=trigger_path, flow_path=flow_path, desc=desc, status=status)
    return(object)
}

##
if(FALSE){

    #q.obj <- queue(type="torque")
    q2 <- queue(object=q.obj,cpu=5)

    replace_slots(q.obj,cpu=4,name="newname")

    ## class(q)
    ## test_queue(q, verbose=TRUE)
    ## hpcc.command.format <- "#{CMD} | qsub -N #{NAME} -q #{QUEUE} -l #{NODES}:#{PPN} -l #{WALLTIME} -S /bin/bash -d #{HOME} -V -e #{STDERR} -o #{STDERR} -m ae -M #{EMAIL}"

    source("~/Dropbox/public/github.flow/R/generic.R")
    source("~/Dropbox/public/github.flow/R/class-def.R")
    q_obj <- queue(type="torque")
    debug(job)
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

