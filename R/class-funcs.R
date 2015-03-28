
#' @export
#' @docType methods
#' @rdname submit_job-methods
#' @title submit_job
#' @description submit_job
#' @param j_obj Object of calls \link{job}
#' @param f_obj Object of calls \link{flow}
#' @param execute A \code{logical} vector suggesting whether to submit this job
#' @param verbose logical
#' @param wd working direcotry
#' @param job_id job id
#' @param ... not used
#' @examples \dontrun{
#' submit_job(j_obj = j_obj, f_obj = f_obj, execute = FALSE,
#' verbose = TRUE, wd = wd, job_id = job_id)
#' }
setGeneric("submit_job", function (j_obj, f_obj, ...){
  standardGeneric("submit_job")
})

setGeneric("submit_flow", function (f_obj, ...){
  standardGeneric("submit_flow")
})

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @title test_queue
#' @description This function attempts to test the submission of a job to the queue.
#' We would first submit one single job, then submit another with a dependency to see if configuration works. This would create a folder in home called 'flows'.
#' @param q_obj queue object
#' @param verbose toggle
#' @param ... These params are passed onto \code{queue}. \code{?queue}, for more information
#' @export
#' @examples
#' \dontrun{
#' test_queue(q_obj = q_obj, ... = ...)}
test_queue <- function(q_obj, verbose = TRUE, ...){
  if(missing(q_obj)){
    type <- readline("What type of cluster is this? Possible values: lsf, torque, sge\n>>> ");
    queue_prompt <- paste0("What queue does your group usually use?\n",
                           "For intitutions this could be",
                           "long, medium, normal, priority.\n",
                           "Not sure? Its best to consult your sysadmins.\n>>> ")
    queue <- readline(prompt = queue_prompt)
    extra <- readline(prompt = "extra options to the queue\n>>>")
    q_obj <- queue(type = type, queue = queue, extra_opts = extra, ...)
  }
  j_obj1 <- job(cmds = 'sleep 60', q_obj = q_obj, name = 'job1')
  #cat("Submitted first job with script:", j_obj1@script, "\n")
  j_obj2 <- job(cmds = 'sleep 60', q_obj = q_obj, name = 'job2',
                previous_job = 'job1', dependency_type = "serial")
  #cat("Submitted second job with script:", j_obj2@script, "\n")
  if(verbose) cat("Creating a 'flow' of two jobs. Check 'flows' folder in your home for a",
                  "new directory called test_....\n",
                  "You may also do bjobs/qstat or a respective command for your",
                  "scheduler to look at these jobs.\n\n\n")
  f_obj <- flow(jobs = list(j_obj1, j_obj2), desc = "test", flow_base_path = "~/flows")
  tmp <- submit_flow(f_obj, execute = TRUE, make_flow_plot = FALSE, verbose = TRUE)
  cat("Flow path:\t", tmp@flow_path, "\n")
  cat("First job ID: \t", tmp@jobs[[1]]@id, "\n")
  cat("Second (dependent) job ID:\t", tmp@jobs[[2]]@id, "\n")
  cat("Path to logs (1):\t", tmp@jobs[[1]]@stdout, "\n")
  cat("Path to logs (2):\t", tmp@jobs[[2]]@stdout, "\n")
  ## cmd.0 <- .create_queue_cmd(j_obj)
  ## if(verbose) cat("An example command string looks like:\n", cmd.0)
  ## cmd <- sprintf("echo 'sleep 1' | %s", cmd.0)
  ## if( verbose ) print (cmd)
  ## system(cmd)
  return(tmp)
}
#debug(test_queue)


#' @title create_queue_cmd
#' @description This is a flow interal functions used to create a command used to submit jobs to the cluster.
#' @aliases create_queue_cmd
#' @param j_obj object of class \link{job}
#' @param file This is the path to the file to run
#' @param index among cmds defined in \code{j_obj}, which index does this \code{file} belong to. A numeric vector of length 1. This is to fetch dependency from previous job.
#' @param ... Not used
#' @keywords internal
#' @examples \dontrun{
#' .create_queue_cmd(j_obj = j_obj, file = file, index = index, ... = ...)
#' }
create_queue_cmd <- function(j_obj, file, index, ...){
  ## ----- this job depends on multiple jobs. create a string with multiple job ids
  ## ----- introduce possibility that the jobid is empty, or missing especially for reruns
  if(j_obj@dependency_type=="gather"){
    ## dependency may be a list with few elements: multi jobs
    ## multiple lists with length 1 each
    ## easiest to unlist, and WAIT for ALL of them
    if(j_obj@type=="torque")
      dependency <- sprintf("-W depend=afterok:%s", paste(unlist(j_obj@dependency), collapse = ":"))
    else if(j_obj@type=="lsf")
      #dependency <- sprintf("-w '%s'", paste(j_obj@dependency, collapse=" && "))
      dependency <- sprintf("-w '%s'", paste(unlist(j_obj@dependency), collapse = " && "))
  }else if (j_obj@dependency_type=="serial"){
    ## if submission is scatter and dependency is serial, do burst
    ## basically recycle the dependency for all the subsequent jobs
    if(length(j_obj@dependency)==1) index=1 ## recycle the first into the rest
    if(j_obj@type=="torque")
      dependency <- sprintf("-W %s",paste(" depend=afterok:", j_obj@dependency[[index]], sep="", collapse=":"))
    else if(j_obj@type=="lsf")
      dependency <- sprintf("-w '%s'", paste(j_obj@dependency[[index]], collapse=" && "))
  }else if (j_obj@dependency_type=="burst"){
    ## if submission is scatter and dependency is serial, do burst
    index=1
    if(j_obj@type=="torque")
      dependency <- sprintf("-W %s",paste(" depend=afterok:", j_obj@dependency[[index]], sep="", collapse=":"))
    else if(j_obj@type=="lsf")
      dependency <- sprintf("-w '%s'", paste(j_obj@dependency[[index]], collapse=" && "))
  }else{
    dependency <- ""
  }
  ## this might be the case if re-run, when only a subset of jobs are to be rerun
  if(length(j_obj@dependency) == 0){
    dependency <- ""
  }
  l <- slots_as_list(j_obj, names=slotNames("queue"))
  ## dependency initially is a list which can have multiple values
  l <- l[! names(l) %in% c("format","type", "dependency")] ### ignore a few of the slots
  ## dependency here is a string according to the policies of the cluster platform
  l <- c(l, dependency=dependency) ## add dependency to the list
  names(l) = toupper(names(l)) ## get list of slots
  l <- c(l, "CMD"=file)
  if(FALSE){ ##finding an alternative to interal call
    .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH if we dont use internal they change temporarily
  }
  do.call(Sys.setenv, l)
  ##cmd <- system(sprintf("eval echo %s ",j_obj@format),intern=TRUE)
  cmd <- system(sprintf("echo %s ", j_obj@format),intern=TRUE)
  return(cmd=cmd)
}

.create_queue_cmd=create_queue_cmd
setMethod("create_queue_cmd", signature(j_obj = "job", file="character"), definition=.create_queue_cmd)

#### --------------------- submit job as part of a flow, this would be called from function flow
.submit_job <- function (j_obj, f_obj, execute = FALSE, verbose = FALSE, wd, job_id,...){
  ## ========= create the name of the job with its index in the supplied flow
  if(!j_obj@status %in% c("processed","submitted","running","completed","error"))
    j_obj@jobname <- sprintf("%03d.%s", job_id,j_obj@name)
  wd <- file.path(f_obj@flow_path, j_obj@jobname) ## j_obj@name: is indexed
  if(!file.exists(wd)) dir.create (wd, recursive=TRUE, showWarnings=FALSE);
  trigger_path=f_obj@trigger_path=file.path(f_obj@flow_path,"trigger") ## comes from the flow
  if(!file.exists(trigger_path)) dir.create(trigger_path, showWarnings=FALSE)
  j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- file.path(dirname(wd),"tmp") ## FLOWBASE
  ## ------------ this would be based on number of submission type
  if(j_obj@submission_type %in% c("serial")){
    j_obj@cmds <-  paste("## ------", names(j_obj@cmds),
                         "\n", j_obj@cmds, "\n\n", collapse="")
  }
  ## if(j_obj@submission_type %in% c("scatter")){
  files <- sprintf("%s/%s_cmd_%s.sh",wd, j_obj@name, 1:length(j_obj@cmds))
  ## ------- do this for all commands (in case of scatter)
  jobids <- sapply(1:length(j_obj@cmds), function(i){
    ## -------   make a long job name to capture the run
    obj <- j_obj;
    obj@jobname <- sprintf("%s_%s-%s",j_obj@jobname,basename(f_obj@flow_path),i)
    cmd <- .create_queue_cmd(obj, file=files[i], index=i)
    ## ------- make the script; add support for other shells, zsh etc OR detect shell
    beforescript <- c("#!/bin/env bash",
                      sprintf("## %s", cmd),
                      sprintf("touch %s/trigger/trigger_%s_%s.txt",
                              f_obj@flow_path, j_obj@jobname,i),
                      "echo 'BGN at' `date`")
    afterscript <- c(sprintf("echo $? > %s/trigger/trigger_%s_%s.txt",
                             f_obj@flow_path, j_obj@jobname,i),
                     "echo 'END at' `date`")
    script <- c(beforescript,j_obj@cmds[i], afterscript)
    if(verbose) cat("Submitting using script:\n", cmd, "\n")
    write(script, files[i])
    if(execute){
      jobid <- system(cmd, intern = TRUE)
      return(jobid)
    } ## execute
    return("") ## if no execute return the cmd
  }) ## for loop
  if(j_obj@type=="lsf" & execute)
    j_obj@id <- gsub(".*(\\<[0-9]*\\>).*","\\1", jobids)
  else ## for all other the output is considered to be the jobid
    j_obj@id <- jobids
  ## }## submissiontype
  j_obj@status <- "processed"
  if(execute) j_obj@status <- "submitted"
  return(j_obj)
}


#' @rdname submit_job-methods
#' @aliases submit_job,job,flow,job-method
setMethod("submit_job", signature(j_obj = "job", f_obj = "flow"), definition = .submit_job)


## TESTS
## number of commands in a serial job should match that of dependency
.submit_flow <- function(f_obj, uuid, execute = FALSE,
                         make_flow_plot = TRUE, verbose = FALSE, ...){
  ## the case of resubmission
  if(missing(uuid)){
    uuid = get_unique_id(f_obj@desc)
  }
  if(!f_obj@status %in% c("processed","submitted","running","completed","exit")){
    f_obj@flow_path <- sprintf("%s/%s",f_obj@flow_base_path, uuid)
  }
  ##jobnames <- sapply(f_obj@jobs, function(x) x@name)
  ##names(f_obj@jobs) <- jobnames
  ### ---------- Error handling
  if(length(f_obj@jobs[[1]]@dependency_type) > 0 & f_obj@jobs[[1]]@dependency_type !="none")
    stop("Seems like the first job has a dependency, please check")
  if(!file.exists(file.path(f_obj@flow_path,"tmp"))) ## create if it does not exist
    dir.create(file.path(f_obj@flow_path,"tmp"), showWarnings=FALSE, recursive=TRUE)
  ## loop on jobs
  for(i in 1:length(f_obj@jobs)){
    ## ------ check if there are any dependencies
    previous_job <- f_obj@jobs[[i]]@previous_job
    dep_type = f_obj@jobs[[i]]@dependency_type
    if(length(previous_job)!=0){ ## prev job should not be of length 0. need ., NA, "" for missing
      ## should not be NA OR NULL
      if(!is.na(previous_job[1]) & !is.null(previous_job[1]) & !previous_job[1] %in% c("", "NA", ".")){
        ## f_obj@jobs[[i]]@dependency <- f_obj@jobs[[previous_job]]@id
        ## -------- can have multiple dependencies
        x <- do.call(cbind, lapply(previous_job, function(y)
          f_obj@jobs[[y]]@id))
        f_obj@jobs[[i]]@dependency <- split(x, row(x))
        ## prev_jobs should have length more than 1. And should not be null
      }else if(length(dep_type) > 0 & !dep_type %in% c("none") & previous_job %in% c("", "NA", ".")){
      	## if prev job is null, but depedency is mentioned
      	stop(paste("Previous job name missing for job: ", f_obj@jobs[[i]]@name))
      }
    }
      ## ------ submit the job
    f_obj@jobs[[i]] <- .submit_job(f_obj@jobs[[i]], f_obj, execute=execute, job_id=i, verbose = verbose, ...)
    ## ------ check if this is NOT last job in the flow
    ## if(i < length(f_obj@jobs)){
    ##     next_job <- f_obj@jobs[[i]]@next_job
    ##     if(length(next_job)!=0)     #if we have the next job
    ##         f_obj@jobs[[next_job]]@dependency <- f_obj@jobs[[i]]@id
    ## }
  }
  f_obj@status <- "processed"
  if(execute){
    f_obj@status <- "submitted"
    ## Rscript -e 'flow:::status(\"%s\")
    cat(sprintf("\nFlow has been submitted. Track it from terminal using:\nOR\nflowr status x=%s\n\n",
                f_obj@flow_path, f_obj@flow_path))
    ## dumpt the flow details
  }
  try(dump_flow_details(fobj = f_obj))
  try(save(f_obj, file = sprintf("%s/flow_details.rda", f_obj@flow_path)))
  if(make_flow_plot & length(f_obj@jobs) > 2){
    try(
      .plot_flow(f_obj, detailed = FALSE, pdf = TRUE, type = '1',
                   pdffile = sprintf("%s/%s-flow_design.pdf",f_obj@flow_path, f_obj@name))
      )
  }else{
    if(verbose) cat("Skipping plots...\n")
  }
  return(f_obj)
}

#' @title submit_flow
#' @description submit_flow
#' @aliases submit_flow
#' @param f_obj \code{object} of class \code{flow}.
#' @param uuid \code{character} A character string pointing to the folder (unique) where all the logs and other files are processed. This is optional and defaults to:
#'  \code{FLOW_DESCRIPTION_UUID}, and this folder is typically created in \code{~/flows/FLOW_NAME}.
#'  Refer to \code{desc} and \code{name} paramters of \link{flow}.
#' @param execute \code{logical} whether or not to submit the jobs
#' @param make_flow_plot \code{logical} whether to make a flow plot (saves it in the flow working directory)
#' @param verbose logical.
#' @param ... Any additional parameter are passed on to \link{submit_job} function
#' @export
#' @examples
#' \dontrun{
#' submit_flow(f_obj = f_obj, ... = ...)}
setMethod("submit_flow", signature(f_obj = "flow"), definition = .submit_flow)


#### ----------------------- submit loner job
if(FALSE){
  setMethod("submit_job", signature(j_obj = "job"),
            function (j_obj, execute = FALSE,verbose = TRUE, wd, ...){
              ## if(verbose) cat(j_obj@base_path, j_obj@name, "\n")
              if(missing(wd)){
                wd <- file.path(j_obj@base_path,paste(j_obj@name,
                                                      UUIDgenerate(),sep="_"))
              }
              dir.create(wd, recursive=TRUE, showWarnings = FALSE)
              script <- c(j_obj@cmd, sprintf("echo $? > %s/trigger_%s.txt", wd,j_obj@name))
              file <- sprintf("%s/%s.sh", wd, j_obj@name)
              write(script, file)
              j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- wd
              cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file)
              if (verbose) print(cmd)
              if(execute){
                jobid <- system(cmd, intern = TRUE)
                j_obj@id <- jobid
              }
              return(j_obj)
            })
}

## trace("create_queue_cmd", browser, exit=browser, signature = c("queue","character"));
## cmd <- create_queue_cmd(j_obj, file=files[i])
## untrace("create_queue_cmd", signature = c("queue","character"));

## setMethod("create_queue_cmd", signature(q_obj = "queue"), function (q_obj, ...){
##     if(q_obj@dependency_type=="gather"){
##         if(q_obj@type=="torque")
##             q_obj@dependency <- sprintf("-W depend=afterok:%s",paste(q_obj@dependency, collapse=":"))
##         else if(q_obj@type=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",paste(q_obj@dependency, sep=" && "))
##     }else if (q_obj@dependency_type=="serial"){
##         if(q_obj@type=="torque")
##             q_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",q_obj@dependency[index], sep=""))
##         else if(q_obj@type=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",q_obj@dependency[index])
##     }else{
##         q_obj@dependency <- ""
##     }
##     l <- slots_as_list(q_obj, names=slotNames("queue"))
##     l <- l[! names(l) %in% c("format","type")] ### ignore a few of the slots
##     names(l) = toupper(names(l)) ## get list of slots
##     ## l <- c("CMD"=cmd)
##     .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
##     cmd <- system(sprintf("eval echo %s ",q_obj@format),intern=TRUE)
##     return(cmd=cmd)
## })
## #cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file=files[i])
