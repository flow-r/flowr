
if(FALSE){
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/ESCA/logs/esca-2015-03-09-00-05-36-tfhaScFP/"
  wd = "/rsrch2/iacs/ngs_runs/1412_tcga_normals/BRCA/logs/brca-2015-02-17-12-42-32-MCscE2AW"
}


## experimental

#' @title rerun_flow
#' @description rerun_flow
#' @param x Either path to flow folder or the \link{flow} object which has been 'returned' from \link{submit_flow}.
#' @param execute [logical] whether to execute or not
#' @param kill logical indicating whether to kill the jobs from old flow
#' @details We need path to the flow folder (\code{wd}). The \link{flow} object needs to have upate 'base_path' slow with wd (the path to the flow folder). Also its important to know that we need details regarding the previous submission from flow_details.txt file. Which should typically be in \code{wd}
#' @export
#' @examples \dontrun{
#' rerun_flow(wd = wd, fobj = fobj, execute = TRUE, kill = TRUE)
#' }
#'  @export
rerun_flow <- function(x, execute = TRUE, kill = TRUE){
  if(class(x) == "flow"){
    f_obj = x
    wd = x@flow_path
  }else if(class(x) == "character" & file.exists(x)){
    load(file.path(x, "flow_details.rda"))
    #fobj = f_obj
  }else{
    stop("x does not seems to be a flow object or a correct path to the flow submission")
  }
  det_file = file.path(wd, "flow_details.txt")
  if(!file.exists(det_file)){
    msg = error("no.flow.det.file")
    stop(msg)
  }
  #debug(get_flow_status)
  get_flow_status(x = wd)
  ## subset those which need to be rerun
  flow_status = read.table(det_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  #plot_flow(x = fobj, pdf = TRUE)
  ## ingest everything in the fobj !
  #head(flow_status)
  mods = unique(as.character(flow_status$jobnm))
  if(kill) capture.output(try(kill_flow(wd = wd)), file = file.path(wd, "kill_jobs.out")) ## kill the flow
  fobj2 = f_obj
  fobj2@status = "" ## reset flow status, will be submitted as a independent flow
  for(m in mods){
    fobj@jobs[[m]]@exit_code = subset(flow_status, flow_status$jobnm == m)$exit_code
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
  knitr::kable(rerun)
  fobj2 <- submit_flow(fobj2, execute = execute)
}