## will be extended to flowid later
## x="/scratch/iacs/ngs_runs/*"

#' @title get_flow_status
#' @description get_flow_status
#' @aliases status
#' @param x path to the flow; may be without the uuid
#' @param cores number of cores to use
#' @param out_format passed onto knitr:::kable. supports: markdown, rst, html...
#' @param get_mem_usage under progress, whether to extract mem_usage of jobs
#' @export
#' @importFrom knitr kable
#' @importFrom parallel mclapply
#' @examples
#' \dontrun{
#' get_flow_status(x = x, cores = 6)
#' ## an example for running from terminal
#' flowr status x=path_to_flow_directory cores=6
#' }
get_flow_status <- function(x, cores = 6, out_format = "markdown", get_mem_usage = TRUE){
  ## get the total jobs
  #wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
  wds = get_wds(x)  
  for(wd in wds){
    files_cmd <- list.files(wd, pattern = "cmd", full.names = TRUE, recursive = TRUE)
    ## dirname, JOBNAME_cmd_JOBINDEX
    mat_cmd <- data.frame(do.call(rbind,
                                  strsplit(gsub(".*/(.*)/(.*)_cmd_([0-9]*).sh",
                                                "\\1,\\2,\\3", files_cmd), split = ",")),
                          file = files_cmd,
                          stringsAsFactors = FALSE)
    colnames(mat_cmd) = c("job_id", "job_name", "num", "file")
    #triggers <- sprintf("%s/trigger/trigger_%s_%s.txt", wd, mat_cmd$job_id, mat_cmd$num)
    triggers = sprintf("%s/trigger/trigger_%s_%s.txt", dirname(dirname(files_cmd)),
                       mat_cmd$job_id, mat_cmd$num)
    status <- unlist(mclapply(triggers, function(y){
      if(file.exists(y)){
        tmp <- as.numeric(scan(y, what = "character", quiet = TRUE))
        tmp <- ifelse(length(tmp) > 0, tmp, -1) ## -1 mean not completed
        return(tmp)
      }else
        return(NA)
      #ifelse(length(tmp) < 1 | grepl("Error", tmp), NA, tmp)
    }))
    ## STATUS -1 MEANS started
    mat_cmd = data.frame(mat_cmd, started = !is.na(status), status = status)
    flow_mat = try(update_flow_mat(wd = wd, mat_cmd = mat_cmd))
    jobs_total <- tapply(mat_cmd$job_id, INDEX = mat_cmd$job_id, length)
    jobs_compl <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, function(z) sum(z > -1, na.rm = TRUE)) ## counts no. more than -1
    jobs_status <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, function(z) sum(ifelse(z>0, 1, 0), na.rm = TRUE))
    jobs_started <- tapply(mat_cmd$started, INDEX = mat_cmd$job_id, function(z) sum(z))
    sum <- data.frame(total = jobs_total, started = jobs_started, completed = jobs_compl, exit_status = jobs_status)
    print(paste0("Showing status of: ", wd))
    write.table(sum, file.path(wd, "flow_status.txt"), quote = FALSE, sep = "\t")
    tmp <- knitr::kable(sum, out_format, output = FALSE)
    print(tmp)
  }
  invisible(flow_mat)
  #return(sum)
}


get_wds <- function(x){
  wds = list.files(dirname(x), full.names = TRUE, pattern = basename(x))
  y = file.info(wds)
  rownames(with(y, {subset(y, isdir == TRUE)}))
}

## read and update flow_details status
# wd = "/scratch/iacs/iacs_dep/sseth/flows/JZ/telseq/my_super_flow-2015-02-15-20-11-21-UZOwi8Q2"
update_flow_mat <- function(wd, mat_cmd){
  fl = list.files(wd, pattern = "flow_details.txt", full.names = TRUE)
  flow_mat = read.table(fl, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  rownames(flow_mat) = paste(flow_mat$jobname, flow_mat$job_no, sep = "_")
  rownames(mat_cmd) = paste(mat_cmd$job_id, mat_cmd$num, sep = "_")
  flow_mat$started = mat_cmd[rownames(flow_mat), 'started']## track using rownames
  flow_mat$exit_code = mat_cmd[rownames(flow_mat), 'status']## track using rownames
  flow_mat$file = mat_cmd[rownames(flow_mat),'file']
  write.table(flow_mat, sep = "\t", quote = FALSE, row.names = FALSE, file = fl)
#   head(flow_mat)
#   head(mat_cmd)
  #dim(flow_mat)
  invisible(flow_mat)
}


#' @export
status = get_flow_status


get_flow_memory <- function(){
  
}

# n= number of lines to read
parse_lsf_out <- function(x, scale_time = 1/3600, n = 100){
  text <- scan(x, what = "character", sep = "\t", n = n)
  cpu_time = gsub("\\s|sec\\.", "", strsplit(grep("CPU time", text, value = TRUE), ":")[[1]][2])
  avg_mem = gsub("\\s| MB", "", strsplit(grep("Average Memory", text, value = TRUE), ":")[[1]][2])
  max_mem = gsub("\\s| MB", "", strsplit(grep("Max Memory", text, value = TRUE), ":")[[1]][2])
  max_swap = gsub("\\s| MB", "", strsplit(grep("Max Swap", text, value = TRUE), ":")[[1]][2])
  return(list(cpu_time = as.numeric(cpu_time) * scale_time, avg_mem = avg_mem, max_mem = max_mem, max_swap = max_swap))
}

#' @title get_resources
#' @description get_resources currenty this only works on LSF
#' @param x A character vector of lenth 1. This may be a parent level folder with directories with multiple flow runs.
#' @param odir Output directory to save the results
#' @param \dots other arguments sent to \link{get_resources_lsf}
#' @details If \code{x} is a parent level folder, then resources are summarized for all its child folders.
#' @export
#' @examples \dontrun{
#' get_resources(x = x, odir = ~/tmp)
#' }
#' @import ggplot2
get_resources <- function(x, odir, ...){
  wds = get_wds(x)
  for(wd in wds){
    if(missing(odir)) odir = wd
    try(get_resources_lsf(wd, ...))
    }# for loop
}# function

#' @title get_resources_lsf
#' @description get_resources_lsf
#' @inheritParams get_resources
#' @param wd Path to a flow working directory
#' @param cores Number of cores to use. [Numeric]
#' @param pattern Pattern to use to get lsf stdout files. Defaults to \code{out$}
#' @import reshape2 tools
#' @keywords internal
#' @examples \dontrun{
#' get_resources_lsf(wd = wd, cores = 4, pattern = out\$)
#' }
get_resources_lsf <- function(wd, cores = 4, pattern = "out$"){
  flow_mat = read.table(file.path(wd, "flow_details.txt"), sep = "\t", header = TRUE)
  rownames(flow_mat) = flow_mat$jobid
  #files_cmd <- list.files(wd, pattern = "sh$", full.names = TRUE, recursive = TRUE)
  files_out <- list.files(wd, pattern = pattern, full.names = TRUE, recursive = TRUE)
  jobid = tools::file_path_sans_ext(basename(files_out))
  names(files_out) = jobid
  #     mat_cmd <- data.frame(do.call(rbind,
  #                                   strsplit(gsub(".*/(.*)/.*/(.*)\\.([0-9]*)\\.([0-9]*)\\.output",
  #                                                 "\\1,\\2,\\3", files_out), split = ",")),
  #                           stringsAsFactors = FALSE)
  #colnames(mat_cmd) = c('oprefix', 'jobname', 'num')
  #mat_cmd <- cbind(mat_cmd, outfile = files_out)
  flow_mat$outfile = files_out[as.c(flow_mat$jobid)]
  tmp = mclapply(as.c(flow_mat$outfile), function(i) try(parse_lsf_out(i)), mc.cores = cores)
  resources <- do.call(rbind, tmp)
  mat_res <- cbind(flow_mat, resources);dim(mat_res)
  ## restructure for plotting:
  mat_res$avg_mem = as.numeric(mat_res$avg_mem)
  mat_res$max_mem = as.numeric(mat_res$max_mem)
  mat_res$max_swap = as.numeric(mat_res$max_swap)
  mat_res$cpu = as.numeric(mat_res$cpu)
  mytheme <- theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  dat = reshape2::melt(mat_res, measure.vars = c("avg_mem", "max_mem", "max_swap", "cpu"))
  p <- with(dat, {ggplot(dat, aes(x = jobname, y = value)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme})
  p <- p + facet_wrap(~variable, scales = "free_y")
  ggsave(sprintf("%s/resources_utilized.pdf", wd), p)
}


dump_flow_details <- function(fobj){
  ret <- lapply(1:length(fobj@jobs), function(i){
    ids = fobj@jobs[[i]]@id ## jobid for submission
    deps = fobj@jobs[[i]]@dependency
    deps = sapply(deps, paste, collapse = ";")
    prev = fobj@jobs[[i]]@previous_job ## works for single type jobs
    #ifelse(prev != "") prev = paste(prev, 1:length(fobj@jobs[[prev]]@id), sep = "_")
    job_no = 1:length(ids)
    job_id = paste(fobj@jobs[[i]]@jobname, job_no, sep = "_")
    mat = cbind(jobname = fobj@jobs[[i]]@jobname, jobnm = fobj@jobs[[i]]@name, 
                job_no = job_no, job_sub_id = ids,
                job_id = job_id,prev = prev,
                dependency = ifelse(is.null(unlist(deps)), NA, unlist(deps)), 
                status = fobj@jobs[[i]]@status, exit_code = NA)
  })
  flow_mat = do.call(rbind, ret)
  write.table(flow_mat, sep = "\t", quote = FALSE, row.names = FALSE,
              file = sprintf("%s/flow_details.txt",fobj@flow_path, fobj@name))
  return(file.path(fobj@flow_path))
}

#' kill_flow
#' @param x either path to flow [character] or fobj object of class \link{flow}
#' @param wd path to a specific which needs to be killed
#' @param fobj a object of class \link{flow}
#' @param kill_cmd The command used to kill. Default is 'bkill' (LSF). One can used qdel for 'torque', 'sge' etc.
#' @param jobid_col Advanced use. The column name in 'flow_details.txt' file used to fetch jobids to kill
#' @examples 
#' \dontrun{
#' ## example for terminal
#' flowr kill_flow wd=path_to_flow_directory
#' }
#' @export
kill_flow <- function(x, wd, fobj, kill_cmd = "bkill", jobid_col = "job_sub_id"){
  if(missing(wd)){
    wd = dump_flow_details(fobj)
  }
  det_file = tail(list.files(wd, pattern = "flow_details", full.names = TRUE), 1)
  flow_details = read.table(det_file, sep = "\t", stringsAsFactors = FALSE, header = TRUE)
  cmds <- sprintf("%s %s", kill_cmd, flow_details[,jobid_col])
  tmp <- sapply(cmds, function(x){
    cat(x, "\n")
    system(x, intern = TRUE)
  })
  invisible(tmp)
}

if(FALSE){
  x = "/scratch/iacs/ngs_runs/140917_SN746_0310_AC5GKGACXX/logs/"
  # Rscript -e 'library(flow); get_flow_status("shrna-Z1-2400REF1-ca2caca1-c484-4034-8f7c-d9b07c595095")'
  library(flow);
  setwd("/scratch/iacs/iacs_dep/sseth/flows/TH/MP_Melanoma_PM70")
  #debug(get_flow_status)
  get_flow_status("shrna-2400KinPlusbaseline-ref1")
  ## Rscript -e 'flow:::status("log")'
}


