
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite SQLite
get_connection <- function(path = "/scratch/iacs/iacs_dep/sseth/.flow/flow.sqlite"){
  #   library(sqldf)
  db <- dbConnect(SQLite(), dbname = path)
  return(db)
}


## will be extended to flowid later
## x="/scratch/iacs/ngs_runs/hpcc_140925_SN1440_0200_AC5K27ACXX/log/stage1*"

#' @title get_flow_status
#' @description get_flow_status
#' @aliases flow:::status
#' @param x path to the flow; may be without the uuid
#' @param cores number of cores to use
#' @param out_format passed onto knitr:::kable. supports: markdown, rst, html...
#' @param get_mem_usage under progress, whether to extract mem_usage of jobs
#' @export
#' @importFrom knitr kable
#' @importFrom parallel mclapply
#' @examples
#' \dontrun{
#' get_flow_status(x = x, cores = 6)}
get_flow_status <- function(x, cores = 6, out_format = "rst", get_mem_usage = TRUE){
  ## get the total jobs
  require(parallel)
  wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
  for(wd in wds){
    files_cmd <- list.files(wd, pattern = "cmd", full.names = TRUE, recursive = TRUE)
    ## dirname, JOBNAME_cmd_JOBINDEX
    mat_cmd <- data.frame(do.call(rbind,
                                  strsplit(gsub(".*/(.*)/(.*)_cmd_([0-9]*).sh",
                                                "\\1,\\2,\\3", files_cmd), split = ",")),
                          stringsAsFactors = FALSE)
    colnames(mat_cmd) = c("job_id", "job_name", "num")
    #triggers <- sprintf("%s/trigger/trigger_%s_%s.txt", wd, mat_cmd$job_id, mat_cmd$num)
    triggers = sprintf("%s/trigger/trigger_%s_%s.txt", dirname(dirname(files_cmd)),
                       mat_cmd$job_id, mat_cmd$num)
    status <- unlist(mclapply(triggers, function(y){
      if(file.exists(y))
        tmp <- try(as.numeric(scan(y, what = "character", quiet = TRUE)))
      else
        return(NA)
      #ifelse(length(tmp) < 1 | grepl("Error", tmp), NA, tmp)
    }))
    mat_cmd = data.frame(mat_cmd, status = status)
    jobs_total <- tapply(mat_cmd$job_id, INDEX = mat_cmd$job_id, length)
    jobs_compl <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, function(z) sum(!is.na(z)))
    jobs_status <- tapply(mat_cmd$status, INDEX = mat_cmd$job_id, sum, na.rm = TRUE)
    sum <- data.frame(total = jobs_total, completed = jobs_compl, exit_status = jobs_status)
    #print("Showing status of:");print(wd)
    write.table(sum, file.path(wd, "flow_status.txt"), quote = FALSE, sep = "\t")
    tmp <- knitr::kable(sum, out_format, output = FALSE)
    pirnt(tmp)
  }
  invisible(sum)
  #return(sum)
}

#' @export
status = get_flow_status


get_flow_memory <- function(){

}


parse_lsf_out <- function(x){
  text <- scan(x, what = "character", sep = "\t")
  cpu = gsub("\\s|sec\\.", "", strsplit(grep("CPU time", text, value = TRUE), ":")[[1]][2])
  avg_mem = gsub("\\s| MB", "", strsplit(grep("Average Memory", text, value = TRUE), ":")[[1]][2])
  max_mem = gsub("\\s| MB", "", strsplit(grep("Max Memory", text, value = TRUE), ":")[[1]][2])
  max_swap = gsub("\\s| MB", "", strsplit(grep("Max Swap", text, value = TRUE), ":")[[1]][2])
  return(list(cpu = cpu, avg_mem = avg_mem, max_mem = max_mem, max_swap = max_swap))
}

#' @title .get_flow_memory_ibm
#' @import ggplot2
.get_flow_memory_ibm <- function(x, odir = "~/tmp"){
  wds = list.files(path = dirname(x), pattern = basename(x), full.names = TRUE)
  for(wd in wds){
    #files_cmd <- list.files(wd, pattern = "sh$", full.names = TRUE, recursive = TRUE)
    files_out <- list.files(wd, pattern = "output$", full.names = TRUE, recursive = TRUE)
    mat_cmd <- data.frame(do.call(rbind,
                                  strsplit(gsub(".*/(.*)/.*/(.*)\\.([0-9]*)\\.([0-9]*)\\.output",
                                                "\\1,\\2,\\3", files_out), split = ",")),
                          stringsAsFactors = FALSE)
    colnames(mat_cmd) = c('oprefix', 'jobname', 'num')
    mat_cmd <- cbind(mat_cmd, outfile = files_out)
    resources <- do.call(rbind, mclapply(as.c(mat_cmd$outfile), function(i) parse_lsf_out(i), mc.cores = 5))
    mat_res <- cbind(mat_cmd, resources);dim(mat_res)

    ## restructure for plotting:
    mat_res$avg_mem = as.numeric(mat_res$avg_mem)
    mat_res$max_mem = as.numeric(mat_res$max_mem)
    mat_res$max_swap = as.numeric(mat_res$max_swap)
    mat_res$cpu = as.numeric(mat_res$cpu)

    mytheme <- theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    p <- with(mat_res,
              ggplot(mat_res, aes(x = jobname, y = avg_mem)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme)
    ggsave(sprintf("%s/%s.avg_mem.pdf", odir, basename(wd)), p)

    p <- with(mat_res,
              ggplot(mat_res, aes(x = jobname, y = max_mem)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme)
    ggsave(sprintf("%s/%s.max_mem.pdf", odir, basename(wd)), p)

    p <- with(mat_res,
              ggplot(mat_res, aes(x = jobname, y = max_swap)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme)
    ggsave(sprintf("%s/%s.max_swap.pdf", odir, basename(wd)), p)

    p <- with(mat_res,
              ggplot(mat_res, aes(x = jobname, y = cpu)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme)
    ggsave(sprintf("%s/%s.cpu_time.pdf", odir, basename(wd)), p)
    ##  head(mat_cmd)

  }


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









