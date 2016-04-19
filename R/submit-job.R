
## --- submit job as part of a flow, this would be called from function flow

#' @title Submit a step of a flow
#' @description 
#' Internal function (used by submit_flow), which submit a single step of a flow.
#'
#' @param jobj Object of calls \link{job}
#' @param fobj Object of calls \link{flow}
#' @param execute A \code{logical} vector suggesting whether to submit this job
#' @param verbose logical
#' @param job_id job id
#' @param ... not used
#'
#' @importFrom tools file_path_as_absolute
#' @examples \dontrun{
#' submit_job(jobj = jobj, fobj = fobj, execute = FALSE,
#' verbose = TRUE, wd = wd, job_id = job_id)
#' }
submit_job <- function (jobj, fobj, job_id, execute = FALSE, verbose = FALSE, ...){
  
  ## --- get the trigger path
  ## --- comes from the flow
  trigger_path=fobj@trigger_path=file.path(fobj@flow_path, "trigger")
  if(!file.exists(trigger_path))
    dir.create(trigger_path, showWarnings=FALSE)
  
  # 	## --- create the name of the job with its index in the supplied flow
  # 	if(!fobj@status %in% c("processed", "rerun", "submitted"))
  # 		if(!jobj@status %in% c("processed", "rerun", "submitted", "running","completed", "error"))
  # 			jobj@jobname <- sprintf("%03d.%s", job_id, jobj@name)
  
  ## --- get the working dir for these job(s)
  ## jobj@name: is indexed
  wd <- file.path(fobj@flow_path, jobj@jobname)
  if(!file.exists(wd))
    dir.create (wd, recursive=TRUE, showWarnings=FALSE);
  
  ## --- get the CWD/PWD for all submissions
  jobj@cwd <- file.path(dirname(wd), "tmp") ## FLOWBASE
  
  ## --- if serial, MERGE all commands in ONE file
  if(jobj@submission_type %in% c("serial") & length(jobj@cmds) > 1){
    jobj@cmds <-  paste("## ------", names(jobj@cmds),
                        "\n", jobj@cmds, "\n\n", collapse="")
  }
  
  ## -- from here on use full WD path
  wd = file_path_as_absolute(wd)
  ## --- shell scripts and their respective STDOUT/ERR
  jobj@script = sprintf("%s/%s_cmd_%s.sh", wd, jobj@name, 1:length(jobj@cmds))
  
  ## gsub .sh from end of file
  jobj@stderr = jobj@stdout = gsub(".sh$", ".out", jobj@script)
  jobj@trigger = sprintf("%s/trigger/trigger_%s_%s.txt", fobj@flow_path, jobj@jobname, 1:length(jobj@cmds))
  
  #jobj@stderr <- file.path(wd, jobj@jobname)
  #jobj@stdout <- file.path(wd, jobj@jobname)
  
  ## ---- do this for all commands (in case of scatter)
  jobids <- sapply(1:length(jobj@cmds), function(i){
    ## ---   make a long job name to capture the run
    obj <- jobj;
    obj@jobname <- sprintf("%s_%s-%s", basename(fobj@flow_path), jobj@jobname, i)
    cmd <- render_queue_cmd(jobj = obj, fobj = fobj, index=i)
    
    ## --- write script to file
    ## this if for debugging
    if(verbose > 2)
      message("Submitting using script:\n", cmd, "\n")
    
    ## --- return CMD if local, else jobid
    if(jobj@platform == "local")
      return(cmd)
    if(execute){
      return(system(cmd, intern = TRUE))
    } ## execute
    return('0') ## if no execute return the 0, as JOBID!
  }) ## for loop
  
  ## --- run local and get 0 as jobids
  if(jobj@platform == "local")
    jobids <- run_local(jobids, jobj = jobj, execute = execute)
  #cat("ALERT !! stopping jobs submission. Please don't press Ctrl+C...\n");
  
  ## --- Parse jobids
  if(execute)
    jobj@id <- parse_jobids(jobids, platform = jobj@platform)
  
  ## --- change the status of this job(s) for logs
  jobj@status <- "processed"
  if(execute) jobj@status <- "submitted"
  
  #Sys.sleep(5);
  return(jobj)
}




# render_queue_cmd
#
# @param file path to the output file
# @param jobj job object
# @param index If more than one, which command to focus on. Can be from \code{1:length(cmds)}
# @param fobj flow object
#' @importFrom utils tail
render_queue_cmd <- function(jobj, file, index, fobj){
  
  if(opts_flow$get("verbose") > 1)
    message("=\nWorking on ", jobj@name, " with index ", index)
  
  ## --- get platform of previous job
  prev_plat = try(fobj@jobs[[jobj@previous_job]]@platform, silent = TRUE)
  prev_plat = ifelse(class(prev_plat) == "try-error", "", prev_plat)
  ##    this job depends on multiple jobs.
  ##    create a string with multiple job ids
  ##     introduce possibility that the jobid is empty,
  ##     or missing especially for reruns
  ##     prev LOCAL	OR execute is FALSE
  if(prev_plat == "local" | !fobj@execute){
    dependency <- "" ## no dependency
  }else{
    dependency <- render_dependency(jobj, index = index)
  }
  
  ## this might be the case if re-run, when only a subset of jobs are to be rerun
  if(length(jobj@dependency) == 0){
    dependency <- ""
  }
  
  ## --- get the data to replace in the template
  l <- slots_as_list(jobj, names=slotNames("queue"))
  ## --- dependency initially is a list which can have multiple values
  ## --- ignore a few of the slots
  l <- l[! names(l) %in% c("format", "platform", "dependency")]
  
  ## --- dependency here is a string according to the policies of the cluster platform
  module_cmds = as.character(fobj@module_cmds)
  l <- c(l, dependency=dependency, module_cmds = module_cmds) ## add dependency to the list
  names(l) = toupper(names(l)) ## get list of slots
  l <- c(l, "CMD" = jobj@cmds[index])
  l$STDERR=l$STDOUT=jobj@stdout[index]
  l$TRIGGER = jobj@trigger[index]
  
  ## --- apply whisker.render on {{CMD}}
  ## render user CMD, if it has {{{CPU}}} etc...
  l$CMD = gsub("%>", "}}}", gsub("<%", "{{{", l$CMD, fixed = TRUE))
  l$CMD = whisker_render(l$CMD, data = l)$out
  
  
  ## --- find the relevent conf file(s)
  ##     use the list to replace
  plat_conf = tail(fetch_conf(paste0(class(jobj),".sh"),
                              verbose = FALSE), 1)
  template <- paste(readLines(plat_conf), collapse = "\n")
  #out = whisker.render(template = template, data = l)
  
  ## render HPCC script, if it has {{{CPU}}} etc...
  out = whisker_render(template, data = l)$out
  
  write(x = out, file = jobj@script[index])
  
  if(jobj@platform == "local"){
    cmd <- sprintf("cd %s;%s %s > %s 2>&1;echo 0", ## exit 0, code of job submission
                   jobj@cwd, jobj@submit_exe, jobj@script[index], jobj@stdout[index])
    return(cmd)
  }else{
    if(jobj@platform == "lsf")
      cmd <- sprintf("%s < %s", jobj@submit_exe, jobj@script[index])
    else
      cmd <- sprintf("%s %s", jobj@submit_exe, jobj@script[index])
    
    return(cmd)
    
  }
  
}
create_queue_cmd=render_queue_cmd

#cmd <- .create_queue_cmd(obj, file=files[i], index=i, fobj = fobj)


#' @title 
#' Wrapper around whisker.render with some additional checks
#' 
#' @description 
#' Internal function (used by submit_job), which creates a submission script using 
#' platform specific templates.
#'
#' @description This is a wrapper around \link{whisker.render}
#' @param template template used
#' @param data a list with variables to be used to fill in the template.
#' @importFrom whisker whisker.render
#'
#' @export
whisker_render <- function(template, data) {
  ## --- remove items, missing in data
  mis = which(sapply(data, length) == 0)
  if(length(mis) > 0)
    data = data[-mis]
  
  vars <- unlist(regmatches(template, gregexpr('(?<=\\{\\{)[[:alnum:]_.]+(?=\\}\\})', template, perl=TRUE)))
  #stopifnot(all(vars %in% names(data)))
  
  mis = vars[!vars %in% names(data)]
  
  if(length(mis) > 0)
    stop("Some variables are specfied in script template, but missing in data ",
         paste(mis, collapse = " "))
  out = whisker.render(template, data)
  return(list(out = out, vars = vars, mis = mis))
}



## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##


.create_queue_cmd <- function(jobj, file, index, fobj, ...){
  .Deprecated("render_queue_cmd")
  
  if(jobj@platform == "local"){
    cmd <- sprintf("cd %s;%s %s > %s 2>&1;echo 0",
                   jobj@cwd, jobj@submit_exe, file, jobj@stdout[index])
    return(cmd)
  }
  
  ## --- get platform of previous job
  prev_plat = try(fobj@jobs[[jobj@previous_job]]@platform, silent = TRUE)
  prev_plat = ifelse(class(prev_plat) == "try-error", "", prev_plat)
  
  ## --- this job depends on multiple jobs.
  ## --- create a string with multiple job ids
  ## --- introduce possibility that the jobid is empty,
  ## --- or missing especially for reruns
  
  ## --- prev LOCAL	OR execute is FALSE
  if(prev_plat == "local" | !fobj@execute){
    dependency <- ""
  }else{
    ## --- GATHER
    jobj = jobj
    class(jobj) = jobj@platform
    dependency <- render_dependency(jobj, index = index)
  }
  
  ## this might be the case if re-run, when only a subset of jobs are to be rerun
  if(length(jobj@dependency) == 0){
    dependency <- ""
  }
  
  l <- slots_as_list(jobj, names=slotNames("queue"))
  ## --- dependency initially is a list which can have multiple values
  ## --- ignore a few of the slots
  l <- l[! names(l) %in% c("format","platform", "dependency")]
  ## --- dependency here is a string according to the policies of the cluster platform
  l <- c(l, dependency=dependency) ## add dependency to the list
  names(l) = toupper(names(l)) ## get list of slots
  l <- c(l, "CMD" = file)
  l$STDERR=l$STDOUT=jobj@stdout[index]
  
  if(FALSE){ ##finding an alternative to interal call
    ## set slots in BASH if we dont use internal they change temporarily
    .Internal(Sys.setenv(names(l), as.character(unlist(l))))
  }
  
  ## --- send all the arguments to SHELL
  do.call(Sys.setenv, l)
  cmd <- system(sprintf("echo %s ", jobj@format), intern=TRUE)
  return(cmd=cmd)
}
