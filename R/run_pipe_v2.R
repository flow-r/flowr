

#' Run automated Pipelines
#'
#' @description
#' Run complete pipelines, by wrapping several steps into one 
#' convinient function.
#' 
#' NOTE: please use flowr version 0.9.8.9010 or higher.
#' 
#' In summary, this function performs the following steps:
#' 
#' \itemize{
#' \item the argument \code{x} defines the name of the pipeline. 
#' Say, for example \code{sleep_pipe}.
#'  \item \link{fetch_pipes}: finds the pipeline definition 
#'  (\code{sleep_pipe.R}, \code{sleep_pipe.def} and
#'  \code{sleep_pipe.conf} files)
#'  \item \code{sleep_pipe(\dots)}: Create all the required commands 
#'  (\code{flowmat})
#'   \item \link{to_flow}: Use \code{flowmat} and 
#'   \code{sleep_pipe.def} to create a flow object.
#'   \item \link{submit_flow}: Submit the flow to the cluster.
#' }
#'
#' @param flow_run_path passed onto to_flow. Default it picked up from flowr.conf. Typically this is ~/flowr/runs
#' @param rerun_wd if you need to re-run, supply the previous working dir
#' @param start_from the step to start a rerun from. Intitutively, this is ignored in a fresh run and only used in re-running a pipeline.
#' @param platform what platform to use, overrides flowdef
#' @param execute TRUE/FALSE
#' @param pipe_func name of the pipeline function in `pipe_src`
#' @param pipe_src path to pipeline script
#' @param flow_def flow definition file
#' @param flow_conf flow conf file with various parameters used by the flow
#' @param flowname name for the flow for submission.
#' @param ... passed onto the pipeline function as specified in x
#'
#' @export
#' @importFrom params load_opts read_sheet write_sheet
#'
#' 
run_pipe_v2 <- function(
  pipe_func,
  pipe_src,
  flow_def, 
  flow_conf,
  flowname,
  platform,
  flow_run_path = opts_flow$get("flow_run_path"),
  rerun_wd, 
  start_from,
  execute = FALSE, ...){
  
  # --- source the file and get the main function from it
  if(!file.exists(pipe_src))
    stop("pls check pipe_src")
  source(pipe_src, TRUE)
  
  message("\n> loading confs....")
  ## load default options for the pipeline
  confs = c(
    file.path(path.expand("~"), ".flowr.conf"),
    fetch_conf("flowr.conf"),
    flow_conf)
  confs = na.omit(confs)
  if(!missing(flow_conf))
    confs = c(confs, flow_conf)
  print(kable(as.data.frame(confs)))
  opts_flow$load(confs, verbose = FALSE, check = TRUE)
  
  message("\n> creating flowmat....")
  # crate a flowmat
  out = pipe_src(...)
  
  # fetched from the latest conf file ONLY
  module_cmds = opts_flow$get("module_cmds")
  
  message("\n> stitching a flow object....")
  
  if(missing(rerun_wd)){
    # create a flow object
    fobj = to_flow(x = out$flowmat,
                   def = flow_def,
                   platform = platform,
                   flowname = flowname,
                   module_cmds = module_cmds,
                   flow_run_path = flow_run_path)
    
    # submit the flow
    message("\n--> submitting ...")
    fobj = submit_flow(fobj, execute = execute)
    
  }else{
    
    fobj = rerun(x = rerun_wd, mat = out$flowmat, def = flow_def, start_from = start_from, execute = execute)
    
  }
  
  invisible(fobj)
}







