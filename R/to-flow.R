setGeneric("to_flow", function (j_obj, f_obj, ...){
  standardGeneric("to_flow")
})


#' @title cmds_to_flow
#' @description cmds_to_flow
#' @param cmd.list list of commands
#' @param samplename name of the sample
#' @param infomat a table with resource requirements and mapping of the jobs in this flow
#' @param q_obj a object of class \link{queue}
#' @param flowname name of the flow
#' @param execute whether to submit the flow to the cluster after creation
#' @param flow_base_path base path for log file etc. Basically the main operating folder for this flow.
#' @export
cmds_to_flow <- function(cmd.list,
                         samplename = "",
                         infomat,
                         q_obj = queue(type = "lsf", versbose=FALSE),
                         flowname = "stage2", 
                         execute = FALSE,
                         flow_base_path = "/scratch/iacs/flow_pipe/tmp"){
  ## trim down the list
  cmd.list = lapply(cmd.list, function(y) Filter(function(x) !x == "", y))
  infomat$dep_type = ifelse(infomat$previous_job==".", "none", "serial")
  #infomat$previous_jobs = ifelse(infomat$previous_job==".", NULL, infomat$previous_job) ## prev job null
  #infomat <- cbind(jobnames, sub_type, cpus, prev_jobs, dep_type)
  ## Error handling: missing in info
  missing.info = names(cmd.list)[!names(cmd.list) %in% infomat$jobname]
  if(length(missing.info) > 0){
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
  if(length(missing.prev) > 0)
    stop("\n\nMessage:\nOops issue with infomat.\n",
         "It seems these previous job names are missing from the jobname column:\n",
         "Dependencies are only supported within the same flow.\n",
         missing.prev, "\n")
  if( mean(table(infomat$jobname)) != 1 || mean(table(names(cmd.list))) != 1 )
    stop("\n\nMessage:\nOops issue with infomat/cmd.list.\n",
         "Seem either jobnames in infomat are or that in cmd.list are duplicated\n")
  
  jobs = list()
  for( i in 1:length(cmd.list)){
    #jobs = lapply(1:length(cmd.list), function(i){
    cat(".")
    cmds = cmd.list[[i]]; jobnm = names(cmd.list)[i]
    #cmds = unique(cmds);
    with(infomat, {
      prev_job = unlist(subset(infomat, jobname == jobnm, select = 'previous_job'))
      prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
      cpu = as.numeric(unlist(subset(infomat, jobname == jobnm, select = 'cpu_reserved')))
      walltime = as.character(unlist(subset(infomat, jobname == jobnm, select = 'walltime')))
      memory = as.character(unlist(subset(infomat, jobname == jobnm, select = 'memory_reserved')))
      queue = as.character(unlist(subset(infomat, jobname == jobnm, select = 'queue')))    
      dep_type = as.character(unlist(subset(infomat, jobname == jobnm, select = 'dep_type')))
      sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
    })
    ## guess dep_type
    if(length(prev_job) > 1){
      dep_type = "gather"
    }else if(length(cmd.list[[prev_job]]) == 0){
      dep_type = "none"
    }else if(length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
      dep_type = "serial"
    }else if(length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
      dep_type = "gather"
    }else if(length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
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
  f_obj <- flow(jobs = jobs,
                desc=sprintf("%s-%s", flowname, samplename), name = flowname,
                mode="scheduler", flow_base_path = flow_base_path)
  len = length(jobs)
  #debug(flow:::.submit_flow)
  #mypack:::reload('flow')
  f_obj_uuid <- .submit_flow(f_obj, execute = execute, make_flow_plot = TRUE)
  #   if(sum(flow:::.create_jobs_mat(f_obj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
  #     cat("Plotting...\n")
  #     try(flow:::.plot_flow(x = f_obj, type = '1',
  #                           pdf = TRUE, pdffile = file.path(f_obj_uuid@flow_path, "flow_design.pdf")))
  #   }
  return(f_obj_uuid)
}


