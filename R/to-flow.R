



#' @title
#' Create flow objects
#' 
#' @name to_flow
#' 
#' @aliases flowr
#'
#' @description
#' Use a set of shell commands (flow mat) and flow definiton to create \link{flow} object.
#'
#' @param x this can either to a filename, a data.frame or a list. 
#' In case it is a file name, it should be a tsv file representing a flow_mat. See \link{to_flowmat} for details
#' @param def a flow definition. Basically a table with resource requirements and mapping of the jobs in this flow.
#' See \link{to_flowdef} for details on the format.
#' 
#' @param platform a specifying the platform to use, possible values are local, lsf, torque, moab, sge and slurm
#' This over-rides the platform column in the flowdef. (optional)
#' 
#' @param grp_col name of the grouping column in the supplied flow_mat.
#' See \link{to_flow} for details. Default value is [samplename].
#' @param jobname_col name of the job name columnd in flow_mat. Defalt value is [jobname].
#' @param cmd_col name of the command column name in flow_mat. Default value is [cmd].
#' 
#' @param flowname name of the flow, this is used as part of the execution foldername.
#' A good simple identifier, which does not support any special characters. 
#' Names may use characters (a-z) and numbers (0-9), using underscore (_) as a word seperator.
#' Default value is  [flowname].
#' 
#' @param flow_run_path base path to be used for execution of this flow. 
#' flowr would create a new time-stamped folder in this base path and 
#' use it for logs, scripts etc. 
#' The default is retrived using \code{opts_flow$get("flow_run_path")}.
#' 
#' @param desc Advanced Use. final flow name.
#' 
#' @param submit after creating a flow object, should flowr 
#' also use \link{submit_flow} to perform a dry-run OR real submission.
#' See below for details. Default value is [FALSE]
#' @param execute when calling \link{submit_flow}, 
#' should flowr execute the flow or perform a dry-run. See below for details. Default value is [FALSE].
#' 
#' @param containerize if the flowmat has multiple samples, 
#' flowr creates a creates a new date-stamped folder, and includes all
#' flows in this batch inside it. 
#' This is keeps the logs clean, and containerizes each batch.
#' To disable this behavious set this to FALSE, default is [TRUE].
#' 
#' @param module_cmds A character vector of additional commands, which will be prepended to each script of the flow. 
#' Default is retreived using \code{opts_flow$get("module_cmds")}.
#' 
#' @param ... Supplied to specific functions like \link{to_flow.data.frame}
#' 
#' @param qobj Depreciated, modify cluster templates as explained on 
#' \href{http://docs.flowr.space/install.html#hpcc_support_overview}{docs.flowr.space}.
#' An object of class \link{queue}.
#' 
#' @param verbose A numeric value indicating the amount of messages to produce.
#'  Values are integers varying from 0, 1, 2, 3, .... Please refer to the \link{verbose} page for more details.
#' \code{opts_flow$get("verbose")}
#' 
#'
#' @examples
#' ## Use this link for a few elaborate examples:
#' ## http://docs.flowr.space/flowr/tutorial.html#define_modules
#' 
#' ex = file.path(system.file(package = "flowr"), "pipelines")
#' flowmat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
#' flowdef = as.flowdef(file.path(ex, "sleep_pipe.def"))
#' fobj = to_flow(x = flowmat, def = flowdef, flowname = "sleep_pipe", platform = "lsf")
#'
#' 
#' ## create a vector of shell commands
#' cmds = c("sleep 1", "sleep 2")
#' ## create a named list
#' lst = list("sleep" = cmds)
#' ## create a flowmat
#' flowmat = to_flowmat(lst, samplename = "samp")
#' 
#' ## Use flowmat to create a skeleton flowdef
#' flowdef = to_flowdef(flowmat)
#' 
#' ## use both (flowmat and flowdef) to create a flow
#' fobj = to_flow(flowmat, flowdef)
#' 
#' ## submit the flow to the cluster (execute=TRUE) or do a dry-run (execute=FALSE)
#' \dontrun{
#' fobj2 = submit_flow(fobj, execute=FALSE)
#' fobj3 = submit_flow(fobj, execute=TRUE)
#' 
#' ## Get the status or kill all the jobs
#' status(fobj3)
#' kill(fobj3)
#' }
#' 
#'
#' @details The parameter x can be a path to a flow_mat, or a data.frame (as read by read_sheet).
#' This is a minimum three column table with columns: samplename, jobname and cmd.
#' See \link{to_flowmat} for details.
#'
#'
#'
#' @return
#' Returns a flow object. If execute=TRUE, fobj is rich with information about where and how
#' the flow was executed. It would include details like jobids, path to exact scripts run etc.
#' To use kill_flow, to kill all the jobs one would need a rich flow object, with job ids present.
#'
#' \strong{Behaviour:}{
#' What goes in, and what to expect in return?
#' \itemize{
#' \item submit=FALSE & execute=FALSE: Create and return a flow object
#' \item submit=TRUE & execute=FALSE: dry-run, Create a flow object then, create a structured execution folder with all the commands
#' \item submit=TRUE, execute=TRUE: Do all of the above and then, submit to cluster
#' }
#' }
#' 
#' @seealso \link{to_flowmat}, \link{to_flowdef}, \link{to_flowdet}, \link{flowopts} and \link{submit_flow}
#'
#' @export
to_flow <- function(x, ...) {
  #message("input x is ", class(x))
  UseMethod("to_flow")
  warnings()
}

#' @rdname to_flow
#' @export
is.flow <- function(x){
  class(x)[1] == "flow"
}

#' @rdname to_flow
#' @export
to_flow.character <- function(x, def,
                              grp_col,
                              jobname_col,
                              cmd_col,
                              ...){
  
  message(">        Reading and checking flow mat  ...")
  x = as.flowmat(x,
                 grp_col = grp_col,
                 jobname_col = jobname_col,
                 cmd_col = cmd_col)
  to_flow(x, def, ...)
  
}

#' @rdname to_flow
#' @export
to_flow.flowmat <- function(x, def,
                            
                            flowname,
                            
                            grp_col,
                            jobname_col,
                            cmd_col,

                            submit = FALSE,
                            execute = FALSE,
                            containerize = TRUE,
                            platform,
                            flow_run_path,
                            
                            qobj, 
                            verbose = opts_flow$get("verbose"),
                            
                            ...){
  
  ## --- change all the input columns into character
  x[] <- lapply(x, as.character)

  x = as.flowmat(x,
                 grp_col = grp_col,
                 jobname_col = jobname_col,
                 cmd_col = cmd_col)

  message("> reading and checking flow def ...")
  def = as.flowdef(def)
  
  # --- defaults
  if (missing(flowname)) {
    flowname = "flowname"
    message("> Using flowname default: ", flowname);
  }
  
  if (missing(flow_run_path)) {
    flow_run_path = opts_flow$get("flow_run_path")
    message("> Using flow_run_path default: ", flow_run_path);
  }
  
  
  if(verbose)
    message("> checking flow definition and flow matrix for consistency...")
  
  # ---  COMPARE flowdef and flowmat jobnames
  msg = c("--> flowdef jobs: ", paste(def$jobname, collapse = " "),
          "\n--> flowmat jobs: ", paste(unique(x$jobname), collapse = " "))
  if (mean(!unique(x$jobname) %in% na.omit(def$jobname))) {
    stop("Some jobs in x are not in flow definition\n", msg)
  }
  if (mean(!na.omit(def$jobname) %in% unique(x$jobname))) {
    stop("Some jobs in flowdef are not in flowmat\n", msg)
  }
  
  ## --- Overrides
  if(verbose)
    message(">        Detecting platform...")
  if ("platform" %in% colnames(def)){
    if(verbose)
      message("--> Will use platform from flow definition")
  }
  if (!missing(platform)){
    if(verbose)
      message("--> Platform supplied, this will override defaults from flow definition...")
    def$platform = platform
  }
  if (!missing(qobj)){
    if(verbose){
      message("--> qobj supplied; this will override defaults from flow_definion OR platform variable")
      message("--> Use of qobj is for advanced use only. ",
              "--> Use shell scripts provided here as a template: https://github.com/sahilseth/flowr/tree/master/inst/conf. ",
              "--> You may tweak and save them in ~/flowr/conf.")
    }
  }else{
    qobj = NA
  }
  
  samps = unique(x$samplename)
  if (length(samps) > 1 & containerize){
    flow_run_path = file.path(flow_run_path, get_unique_id(flowname))
    if(verbose & submit)
      message(">         Containerizing ...", 
              "\n--> Detected multiple samples. Subsetting flowmat, would containerize this submission...", 
              "\n--> Using folder: ", flow_run_path)
  }

  fobjs <- lapply(samps, function(samp){
    
    x2 = subset(x, x$samplename == samp)
    if(verbose)
      message(">  working on... ", samp)
    
    ## Compare flowdef and flowmat, AGAIN
    ## in case of more complex flows like fastq_mutect
    msg = c("\n--> flowdef jobs: ", paste(def$jobname, collapse = " "),
            "\n--> flowmat jobs: ", paste(unique(x2$jobname), collapse = " "))
    if (mean(!na.omit(def$jobname) %in% unique(x2$jobname))) {
      stop("Some jobs in flowdef are not in flowmat\n", msg)
    }
    
    ## --- fetch samplename from the flowr_mat
    cmd.list = split.data.frame(x2, x2$jobname)
    desc = paste(flowname, samp, sep = "-")
    fobj = to_flow(x = cmd.list,
                   def = def,
                   desc = desc,
                   flowname = flowname,
                   flow_run_path = flow_run_path,
                   qobj = qobj, ...)
    
    
    # if execute is true, submit has to be TRUE
    if (execute|submit){
      if (execute)
        submit = TRUE
    }
    
    if (submit)
      fobjuuid <- submit_flow(fobj, execute = execute)
    if (execute)
      return(fobjuuid)
    else
      return(fobj)
  })
  
  # if there is only one sample, fobj is returned, else a list of flow objects
  if (length(fobjs) == 1)
    fobjs = fobjs[[1]]
  
  invisible(fobjs)
}

#' @rdname to_flow
#' @export
to_flow.data.frame = function(x, ...){
  # if a data.frame is supplied instead of a flowmat class
  to_flow(as.flowmat(x), ...)
}


## several variables come from global space !
proc_jobs <- function(x,
                      i, 
                      qobj,
                      def,
                      desc,
                      verbose = opts_flow$get("verbose")
                      
){
  
  if(verbose)
    message(".", appendLF = FALSE)
  
  jobnm = def[i, "jobname"]
  cmds = x[[jobnm]]$cmd;
  
  
  ## --- submit def, to get resources for this particular job
  def2 = subset(def, def$jobname == jobnm)
  prev_job = unlist(def2$prev_jobs) ## SHOULD be NA
  if (!is.na(prev_job))
    prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
  d_cpu = unlist(def2$cpu_reserved)
  d_walltime = unlist(def2$walltime)
  d_memory = as.character(unlist(def2$memory_reserved))
  d_queue = unlist(def2$queue)
  d_dep_type = unlist(def2$dep_type)
  d_sub_type = unlist(def2$sub_type)
  d_nodes = unlist(def2$nodes)
  d_jobid = unlist(def2$jobid)
  
  if (!inherits(qobj, "queue")){
    qobj <- queue(platform = unlist(def2$platform), verbose = FALSE)
  }else{
    if(verbose > 1)
      message("using queue object supplied")
  }
  
  #print(qobj@submit_exe)
  
  ## --- getting default for nodes
  if (is.null(d_nodes))
    d_nodes = '1'
  
  
  ##  if cmds are missing; change to echo 0 and make cpu = 1
  d_cpu = ifelse(cmds[1] == ".", 1, d_cpu)
  ## if starts from . echo
  cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1])
  
  if(is.null(cmds))
    stop("command is missing for: ", desc, " jobnm: ", jobnm, 
         "\nPlease check the flowmat and flowdef; this is a show stoppper.\n")
  
  jobj = job(q_obj = qobj,
             name = jobnm,
             jobname = sprintf("%03d.%s", d_jobid, jobnm),
             previous_job = prev_job,
             cmds = cmds,
             dependency_type = d_dep_type,
             submission_type = d_sub_type,
             cpu = d_cpu, queue = d_queue,
             walltime = d_walltime,
             nodes = d_nodes,
             memory = d_memory)
  return(jobj)
}


#' @rdname to_flow
#' @importFrom utils packageVersion
#' @importFrom params kable
to_flow.list <- function(x, def,
                         
                         flowname, 
                         flow_run_path, 
                         desc, 
                         qobj, 
                         
                         module_cmds = opts_flow$get("module_cmds"),
                         
                         verbose = opts_flow$get("verbose"), ...){
  
  ## --- qobj, missing only works for arguments
  # 	if(is.flowmat(x[[1]])){
  # 		warning("to_flow supports a list of commands as a input not list of flowmats.")
  # 		x = do.call(rbind, x)
  # 		fobj = to_flow(x, def, flowname = flowname,
  # 						flow_run_path = flow_run_path, ...)
  # 		return(fobj)
  # 	}
  
  ## x is a list of flow_mat, split by jobname
  ## this list should have three elements
  
  jobs <- lapply(1:nrow(def), function(i){
    proc_jobs(x = x, i = i, qobj = qobj, def = def, verbose = verbose, desc = desc)
  })
  
  fobj <- flow(jobs = jobs,
               desc = desc, 
               name = flowname,
               
               mode = "scheduler",
               
               module_cmds = module_cmds,
               version = as.character(packageVersion("flowr")),
               flow_run_path = flow_run_path)
  
  ## --- check if submission or depedency types were guessed
  if (is.null(def$sub_type) | is.null(def$dep_type)){
    message("Submission/definition types were guessed.",
            "\nThis is really a experimental feature.",
            "\nPlease check the following table.",
            "\nIncase of issues please re-submit specifying them explicitly.")
    mydef = create_jobs_mat(fobj)
    cols = c("jobname",  'prev_jobs',  'dep_type', 'sub_type')
    print(kable(mydef[, cols], col.names=FALSE))
  }
  
  invisible(fobj)
}






















## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##




#setMethod("to_flow", signature(x = "list"), definition = .to_flow.list)
#setMethod("to_flow", signature(x = "data.frame"), definition = .to_flow.data.frame)


# @title cmds_to_flow: DEPRECIATED
# @description Create a \link{flow} object from a list of commands
#
# @param cmd.list list of commands
# @param samplename name of the sample
# @param infomat flowdef
# @param q_obj queue object
# @param flowname name of the flow
# @param execute TRUE/FALSE
# @param flow_run_path outpath
cmds_to_flow <- function(cmd.list,
                         samplename = "",
                         infomat,
                         q_obj = queue(type = "lsf", verbose=FALSE),
                         flowname = "stage2",
                         execute = FALSE,
                         flow_run_path = "/scratch/iacs/flow_pipe/tmp"){
  .Deprecated("to_flow")
  ## trim down the list
  cmd.list = lapply(cmd.list, function(y) Filter(function(x) !x == "", y))
  infomat$dep_type = ifelse(infomat$previous_job==".", "none", "serial")
  #infomat$previous_jobs = ifelse(infomat$previous_job==".", NULL, infomat$previous_job) ## prev job null
  #infomat <- cbind(jobnames, sub_type, cpus, prev_jobs, dep_type)
  ## Error handling: missing in info
  missing.info = names(cmd.list)[!names(cmd.list) %in% infomat$jobname]
  if (length(missing.info) > 0){
    warning("\n\nMessage:\nOops issue with infomat.\n",
            "It seems these job names are missing from the infomat:\n",
            paste(missing.info, collapse="\n"),
            ".\nWill remove them from cmd.list and proceed\n")
    ## removing from commands if missing in infomat
    cmd.list = cmd.list[names(cmd.list) %in% infomat$jobname]
  }
  ## chck if prev jobs have a job defined
  prev_jobs = strsplit(infomat$previous_job, ",")
  missing.prev = infomat$previous_job[!unlist(prev_jobs) %in% c(infomat$jobname, ".", NA, "NA")]
  if (length(missing.prev) > 0)
    stop("\n\nMessage:\nOops issue with infomat.\n",
         "It seems these previous job names are missing from the jobname column:\n",
         "Dependencies are only supported within the same flow.\n",
         missing.prev, "\n")
  if ( mean(table(infomat$jobname)) != 1 || mean(table(names(cmd.list))) != 1 )
    stop("\n\nMessage:\nOops issue with infomat/cmd.list.\n",
         "Seem either jobnames in infomat are or that in cmd.list are duplicated\n")
  
  jobs = list()
  for( i in 1:length(cmd.list)){
    #jobs = lapply(1:length(cmd.list), function(i){
    #message(".")
    cmds = cmd.list[[i]]; jobnm = names(cmd.list)[i]
    #cmds = unique(cmds);
    infomat2 = subset(infomat, infomat$jobname == jobnm)
    #print(knitr:::kable(infomat2))
    prev_job = unlist(infomat2$previous_job)
    prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
    cpu = unlist(infomat2$cpu_reserved)
    walltime = unlist(infomat2$walltime)
    memory = as.character(unlist(infomat2$memory_reserved))
    queue = unlist(infomat2$queue)
    dep_type = unlist(infomat2$dep_type)
    sub_type = unlist(infomat2$sub_type)
    if (length(sub_type) == 0)
      sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
    ## guess dep_type
    if (length(prev_job) > 1){
      dep_type = "gather"
    }else if (length(cmd.list[[prev_job]]) == 0){
      dep_type = "none"
    }else if (length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
      dep_type = "serial"
    }else if (length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
      dep_type = "gather"
    }else if (length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
      dep_type = "burst"
    }
    ##dep_type = unlist(subset(infomat, jobname == jobnm, select = 'sub_type'))
    ##sub_type = "serial"
    ## -------- if cmds are missing; change to echo 0 and make cpu = 1
    cpu = ifelse(cmds[1] == ".", 1, cpu)
    cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1]) ## if starts from . echo
    j = job(q_obj = q_obj, name = jobnm, previous_job = prev_job, cmds = cmds,
            dependency_type = dep_type, submission_type = sub_type,
            cpu = cpu, queue = queue,
            walltime = walltime, memory = memory)
    jobs = c(jobs, j)
  }
  fobj <- flow(jobs = jobs,
               desc=sprintf("%s-%s", flowname, samplename), name = flowname,
               mode="scheduler", flow_run_path = flow_run_path)
  len = length(jobs)
  #debug(flow:::.submit_flow)
  #mypack:::reload('flow')
  fobj_uuid <- submit_flow(fobj, execute = execute, make_flow_plot = TRUE)
  #   if (sum(flow:::create_jobs_mat(fobj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
  #     cat("Plotting...\n")
  #     try(flow:::plot_flow(x = fobj, type = '1',
  #                           pdf = TRUE, pdffile = file.path(fobj_uuid@flow_path, "flow_design.pdf")))
  #   }
  return(fobj_uuid)
}



