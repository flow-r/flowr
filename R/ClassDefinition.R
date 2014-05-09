## A class that contains bam file information

## Copyright 2013, Sahil Seth, all rights reserved
## A few functions to supplement those already in this package.

#### ----------------------- Declaring new classes
setClass("queue", representation(submit.cmd = "character", ## submit job
                                 queue = "character", ## type of queue
                                 nodes = "numeric", ## number of nodes
                                 cpu = "numeric",
                                 dependency = "character", ## job id
                                 walltime = "character", ## walltime
                                 home = "character", ## home
                                 stderr = "character", ## stderr
                                 stdout = "character",
                                 email = "character", ## email
                                 type = "character",
                                 format = "character"))

## represents a single job
setClass("job", representation(queue = "queue",
                               cmd = "character")) # a string of cmd to run
                               
setClass("flow", representation(jobs = "list",
                                mode = "character"))


#### ---------------------- Functions to create new classes
queue <- function(submit.cmd="qsub",queue="iacs",nodes=1,cpu=24,dependency="",walltime="72:00:00",home="$HOME",
                  stderr="$HOME/tmp/flow",stdout="$HOME/tmp/flow",email="$USER",type="torque.mda",format=""){  
  object <- new("queue", submit.cmd=submit.cmd,queue=queue,nodes=nodes,cpu=cpu,dependency=dependency,walltime=walltime,
      home=home,stderr=stderr,stdout=stdout,email=email,type=type,format=format)
  return(object)
}

job <- function(queue,cmd){
  object <- new("job",queue=queue,cmd=cmd)
}

submit.job

## 
if(FALSE){
  hpcc.command.format <- "#{CMD} | qsub -N #{NAME} -q #{QUEUE} -l #{NODES}:#{PPN} -l #{WALLTIME} -S /bin/bash -d #{HOME} -V -e #{STDERR} -o #{STDERR} -m ae -M #{EMAIL}"
  q.obj <- queue()
  job <- job(queue=q.obj,cmd="sleep 2")
  
  
}
  