

get_connection <- function(path = "/scratch/iacs/iacs_dep/sseth/.flow/flow.sqlite"){
  #   library(sqldf)
  db <- dbConnect(SQLite(), dbname = path)
  return(db)
}


## will be extended to flowid later
#' @title get_flow_status
#' @description get_flow_status
#' @aliases flow:::status
#' @param x path to the flow; may be without the uuid
#' @param cores
#' @export
#' @importFrom knitr kable
#' @importFrom parallel mclapply
#' @examples
#' \dontrun{
#' get_flow_status(x = x, cores = 6)}
get_flow_status <- function(x, cores = 6, out_format = "rst"){
  ## get the total jobs
  wds = list.files(path = dirname(x), pattern = basename(x))
  for(wd in wds){
    files_cmd <- list.files(wd, pattern = "cmd", full.names = TRUE, recursive = TRUE)
    mat_cmd <- data.frame(do.call(rbind, 
                                  strsplit(gsub(".*/(.*)/(.*)_cmd_([0-9]*).sh", 
                                                "\\1,\\2,\\3", files_cmd), split = ",")), 
                          stringsAsFactors = FALSE)
    colnames(mat_cmd) = c("job_id", "job_name", "num")
    triggers <- sprintf("%s/trigger/trigger_%s_%s.txt", wd, mat_cmd$job_id, mat_cmd$num)
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
    print(wd)
    knitr::kable(sum, out_format)
  }
  invisible(sum)
}

#' @export
status = get_flow_status


if(FALSE){
  # Rscript -e 'library(flow); get_flow_status("shrna-Z1-2400REF1-ca2caca1-c484-4034-8f7c-d9b07c595095")'
  library(flow); 
  setwd("/scratch/iacs/iacs_dep/sseth/flows/TH/MP_Melanoma_PM70")
  #debug(get_flow_status)
  get_flow_status("shrna-2400KinPlusbaseline-ref1") 
}









