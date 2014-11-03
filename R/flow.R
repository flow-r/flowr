#' @title convert command matrix to specific flow
#' @description
cmds_to_flow <- function(cmd.list,
                         samplename = "",
                         infomat,
                         q_obj = queue(type = "lsf", queue = "normal"),
                         flowname = "stage2", execute = FALSE,
                         flow_base_path = "~/tmp"){
  #mypack:::reload('flow')
  cmd.list = as.list(cmd.list)
  ## trim down the list
  cmd.list = lapply(cmd.list, function(y) Filter(function(x) !x == "", y))
  infomat$dep_type = ifelse(infomat$previous_job==".", "none", "serial")
  #infomat$previous_jobs = ifelse(infomat$previous_job==".", NULL, infomat$previous_job) ## prev job null
  #infomat <- cbind(jobnames, sub_type, cpus, prev_jobs, dep_type)
  jobs = lapply(1:length(cmd.list), function(i){
    cat(".")
    cmds = cmd.list[[i]]; jobnm = colnames(cmd_mat)[i]
    #cmds = unique(cmds);
    prev_job = unlist(subset(infomat, jobname == jobnm, select = 'previous_job'))
    prev_job = strsplit(prev_job, ",")[[1]]
    cpu = unlist(subset(infomat, jobname == jobnm, select = 'cpu_reserved'))
    dep_type = unlist(subset(infomat, jobname == jobnm, select = 'dep_type'))
    sub_type = ifelse(length(cmds) > 1, "scatter", "serial")
    ##
    if(length(cmd.list[[prev_job]]) == 0){
      dep_type = "none"
    }else if(length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
      dep_type = "serial"
    }else if(length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
      dep_type = "gather"
    }else if(length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
      dep_type = "burst"
    }
    #dep_type = unlist(subset(infomat, jobname == jobnm, select = 'sub_type'))
    #sub_type = "serial"
    ## -------- if cmds are missing; change to echo 0 and make cpu = 1
    cpu = ifelse(cmds[1] == ".", 1, cpu)
    cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1]) ## if starts from . echo
    j = job(q_obj = q_obj, name = jobnm, previous_job = prev_job, cmds = cmds,
            dependency_type = dep_type, submission_type = sub_type, cpu = cpu)
  })
  f_obj <- flow(jobs = jobs,
                desc=sprintf("%s-%s", flowname, samplename), name = flowname,
                mode="scheduler", flow_base_path = flow_base_path)
  len = length(jobs)
  #debug(flow:::.submit_flow)
  #mypack:::reload('flow')
  f_obj_uuid <- flow:::.submit_flow(f_obj, execute = execute, make_flow_plot = FALSE)
  if(sum(flow:::.create_jobs_mat(f_obj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
    cat("Plotting...\n")
    try(flow:::.plot_flow(x = f_obj, type = '2', height = len*3, width = len*2.5, box.cex = 4, cex.txt = 4,
                          segment.from = 0, segment.to = 0.99, arr.length = 1.5, arr.width = 1, arr.type = "triangle",
                          arr.pos = 0.95,
                          pdf = TRUE, pdffile = file.path(f_obj_uuid@flow_path, "flow_design.pdf")))
  }
  return(f_obj_uuid)
}


### some infomat funcs
get_resources <- function(cmd_name, flow_def){
  
}
