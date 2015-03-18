
if(FALSE){
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/ESCA/logs/esca-2015-03-09-00-05-36-tfhaScFP/"
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/BRCA/logs/brca-2015-02-17-12-42-32-MCscE2AW"
}


## experimental

#' @title rerun_flow
#' @description rerun_flow
#' @param wd
#' @param fobj
#' @param execute
#' @param kill logical indicating whether to kill the jobs from old flow
#' @export
#' @examples \dontrun{
#' rerun_flow(wd = wd, fobj = fobj, execute = TRUE, kill = TRUE)
#' }
#'  @export
rerun_flow <- function(wd, fobj, execute = TRUE, kill = TRUE){
  if(!missing(wd))
    load(file.path(wd, "flow_details.rda"))
  fobj = f_obj
  #debug(get_flow_status)
  get_flow_status(x = wd)
  ## subset those which need to be rerun
  flow_status = read.table(file.path(wd, "flow_details.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  #plot_flow(x = fobj, pdf = TRUE)
  ## ingest everything in the fobj !
  #head(flow_status)
  mods = unique(as.character(flow_status$jobnm))
  if(kill) capture.output(try(kill_flow(wd = wd)), file = file.path(wd, "kill_jobs.out")) ## kill the flow
  fobj2 = fobj
  fobj2@status = "" ## reset flow status, will be submitted as a independent flow
  for(m in mods){
    fobj@jobs[[m]]@exit_code = subset(flow_status, jobnm == m)$exit_code
    ## if we do not know the exit code, we redo
    redo = !fobj@jobs[[m]]@exit_code == 0;redo = ifelse(is.na(redo), TRUE, redo)
    ## need to subset CMDS, 
    fobj2@jobs[[m]]@cmds = fobj2@jobs[[m]]@cmds[ redo ]
    fobj2@jobs[[m]]@dependency = list() ## dependent job ids
    fobj2@jobs[[m]]@id = vector(mode = "character")
    fobj2@jobs[[m]]@exit_code = vector(mode = "numeric")
  }
  ## subset and get jobs which failed
  rerun = cbind(module = mods, rerun = unlist(lapply(mods, function(m) length(fobj2@jobs[[m]]@cmds))))
  knitr:::kable(rerun)
  fobj2 <- submit_flow(fobj2, execute = execute)
}