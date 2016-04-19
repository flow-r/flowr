
#setClass("flowdef", contains = "data.frame")
#http://www.carlboettiger.info/2013/09/11/extending-data-frame-class.html



#' @rdname to_flowdef
#' @aliases flowdef to_flowdef definition
#' 
#' @title
#' Flow Definition defines how to stich steps into a (work)flow.
#'
#' @description 
#' This function enables creation of a skeleton flow definition with several default values, using a 
#' flowmat.
#' To customize the flowdef, one may supply parameters such as sub_type and dep_type upfront.
#' As such, these params must be of the same length as number of unique jobs using in the flowmat.
#' 
#' {
#' Each row in this table refers to one step of the pipeline. 
#' It describes the resources used by the step and also its relationship with other steps, 
#' especially, the step immediately prior to it.
#' } <br><br>
#' 
#' \strong{Submission types:} 
#' \emph{This refers to the sub_type column in flow definition.}<br>
#' 
#' Consider an example with three steps A, B and C. 
#' A has 10 commands from A1 to A10, similarly B has 10 commands B1 through B10 and 
#' C has a single command, C1.
#' Consider another step D (with D1-D3), which comes after C.
#' 
#' step (number of sub-processes)
#' A (10)   ----> B (10)  -----> C (1) -----> D (3)
#' 
#' 
#'  
#' 
#' \itemize{
#' \item \code{scatter}: submit all commands as parallel, independent jobs. 
#' 
#' 	\emph{Submit A1 through A10 as independent jobs}
#' 	\item \code{serial}: run these commands sequentially one after the other. 
#' 	
#' 	- \emph{Wrap A1 through A10, into a single job.}
#'}
#'
#' \strong{Dependency types}
#'
#' \emph{This refers to the dep_type column in flow definition.}
#' 
#' \itemize{
#' \item \code{none}: independent job.
#' 		\itemize{\item \emph{Initial step A has no dependency}}
#' 	\item \code{serial}: \emph{one to one} relationship with previous job. 
#' 	\itemize{ \item \emph{B1 can start as soon as A1 completes, and B2 starts just after A2 and so on.}}
#' 	\item \code{gather}: \emph{many to one}, wait for \strong{all} commands in the previous job to finish then start the  current step. 
#' 	\itemize{\item \emph{All jobs of B (1-10), need to complete before C1 starts}}
#' 	\item \code{burst}: \emph{one to many} wait for the previous step which has one job and start processing all cmds in the current step. 
#' 	
#' 	- \emph{D1 to D3 are started as soon as C1 finishes.}
#' }
#' 
#' @format
#' This is a tab separated file, with a minimum of 4 columns:<br>
#' 
#' \emph{required columns}:<br>
#' \itemize{
#' 
#' \item{\code{jobname}}: Name of the step
#' 
#' \item{\code{sub_type}}: Short for submission type, 
#'  refers to, how should multiple commands of this step be submitted. Possible values are `serial` or `scatter`. 
#'  
#' \item{\code{prev_jobs}}: Short for previous job, this would be the jobname of the previous job. 
#' This can be NA/./none if this is a independent/initial step, and no previous step is required for this to start. 
#' Additionally, one may use comma(s) to define multiple previous jobs (A,B).
#' 
#' \item{\code{dep_type}}: Short for dependency type, 
#' refers to the relationship of this job with the one defined in `prev_jobs`. 
#' This can take values `none`, `gather`, `serial` or `burst`.
#' 
#' }
#' 
#' \emph{resource columns} (recommended):<br>
#' 
#' Additionally, one may customize resource requirements used by each step.
#' The format used varies and depends to the computing platform. Thus its best to refer to 
#' your institutions guide to specify these.
#' 
#' \itemize{
#' 	\item{\code{cpu_reserved}} integer, specifying number of cores to reserve [1]
#'	\item{\code{memory_reserved}} Usually in KB [2000]
#'	\item{\code{nodes}} number of server nodes to reserve, most tools can only use multiple cores on
#'	a \strong{single} node [1]
#'	\item{\code{walltime}} maximum time allowed for a step, usually in a HH:MM or HH:MM:SS format. [1:00]
#'	\item{\code{queue}} the queue to use for job submission [short]
#' }
#'
#' @param x can a path to a flowmat, flowmat or flow object.
#' @param sub_type submission type, one of: scatter, serial. Character, of length one or same as the number of jobnames
#' @param dep_type dependency type, one of: gather, serial or burst. Character, of length one or same as the number of jobnames
#' @param prev_jobs previous job name
#' @param queue Cluster queue to be used
#' @param platform platform of the cluster: lsf, sge, moab, torque, slurm etc.
#' @param memory_reserved amount of memory required.
#' @param cpu_reserved number of cpu's required. [1]
#' @param nodes if you tool can use multiple nodes, you may reserve multiple nodes for it. [1]
#' @param walltime amount of walltime required
#' @param guess should the function, guess submission and dependency types. See details. 
#' @inheritParams to_flow
#' @param ... not used
#'
#' @details 
#' 
#' \strong{NOTE:} Guessing is an experimental feature, please check the definition carefully. 
#' it is provided to help but not replace your best judgement. <br>
#' 
#' Optionally, one may provide the previous jobs and flowr can try guessing the appropriate 
#' submission and dependency types. If there are multiple commands, default is submitting them as 
#' scatter, else as serial. Further, if previous job has multiple commands and current job has single;
#' its assumed that all of the previous need to complete, suggesting a gather type dependency.
#' 
#' @importFrom params kable
#' 
#' @export
#' 
#' @examples
#' # see ?to_flow for more examples
#' 
#' # read in a tsv; check and confirm format
#' ex = file.path(system.file(package = "flowr"), "pipelines")
#' 
#' # read in a flowdef from file
#' flowdef = as.flowdef(file.path(ex, "sleep_pipe.def"))
#' 
#' # check if this a flowdef
#' is.flowdef(flowdef)
#' 
#' # use a flowmat, to create a sample flowdef
#' flowmat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
#' to_flowdef(flowmat)
#' 
#' # change the platform
#' to_flowdef(flowmat, platform = "lsf")
#' 
#' # change the queue name
#' def = to_flowdef(flowmat, 
#'  platform = "lsf", 
#'  queue = "long")
#' plot_flow(def)
#' 
#' # guess submission and dependency types
#' def2 = to_flowdef(flowmat, 
#'  platform = "lsf", 
#'  queue = "long", 
#'  guess = TRUE)
#' plot_flow(def2)
#' 
#'
#' 
to_flowdef <- function(x, ...){
  #message("input x is ", class(x)[1])
  UseMethod("to_flowdef")
  warnings()
}

# detect_dep_type
# @param x job object
# @param ncmds a string of commands
# @param prev_job previous job name
detect_sub_type <- function(ncmds){
  sub_type = as.character(ifelse(ncmds > 1, "scatter", "serial"))
  return(sub_type)
}

# detect_dep_type
# @param x job object
# @param cmds a string of commands
# @param prev_job previous job name
detect_dep_type <- function(ncmds, prev_job, npcmds){
  
  # multiple previos jobs
  if (length(prev_job) > 1) {
    dep_type = "gather"
    
    # no previous job
  }else if (prev_job == "none") {
    dep_type = "none"
    
    # both prev. and current have same number of commands
    # if same length, serial
  }else if (ncmds == npcmds[1] & length(prev_job) == 1) {
    dep_type = "serial"
    
    # multiple previous, each streaming into this
  }else if (all(ncmds == npcmds) & length(prev_job) > 1) {
    dep_type = "serial"
    
    # if prevous job were more than current, a safety net
  }else if (any( npcmds > ncmds)) {
    dep_type = "gather"
    
    # single previous and multiple cmds in current
  }else if (npcmds == 1 & ncmds > 1) {
    dep_type = "burst"
    
  }else{
    message("Number of cmds: ", ncmds, " and previous cmds: ", npcmds)
    message("Could not decide a depedency type; this was too confusing to me. Help from Stackoverflow/Github?")
    dep_type = "gather"
  }
  
  return(dep_type)
  
}

guess_sub_dep <- function(mat, def){
  
  # expand rows with multiple dependencies
  long_def = split_multi_dep(def)
  
  lst <- lapply(1:nrow(def), function(i){
    
    # get current jobname, previous jobname(s)
    # as well as current and previous commands
    jobnm = def$jobname[i]
    message("--> working on ", jobnm)
    ncmds = nrow(subset(mat, mat$jobname == jobnm))
    prev_job = subset(def, def$jobname == jobnm)$prev_jobs
    
    # get number of commands in each of the previous steps
    prevmat = subset(mat, mat$jobname %in% prev_job)
    npcmds = sapply(split.data.frame(prevmat, prevmat$jobname), nrow)
    
    d_sub_type <- detect_sub_type(ncmds = ncmds)
    d_dep_type <- detect_dep_type(prev_job = prev_job, ncmds = ncmds, npcmds = npcmds)
    
    data.frame(sub_type = d_sub_type, dep_type = d_dep_type, stringsAsFactors = FALSE)
  })
  
  tmp = do.call(rbind, lst)
  
  def$sub_type = tmp$sub_type
  def$dep_type = tmp$dep_type
  
  return(def)
}

#' @rdname to_flowdef
#' @export
to_flowdef.flowmat <- function(x,
                               sub_type,
                               dep_type,
                               prev_jobs,
                               queue = "short",
                               platform = "torque",
                               memory_reserved = "2000", ## in MB
                               cpu_reserved = "1",
                               nodes = "1",
                               walltime = "1:00",
                               guess = FALSE,
                               verbose = opts_flow$get("verbose"), ...){
  
  if(verbose)
    message("Creating a skeleton flow definition")
  jobnames <- unique(x$jobname)
  if(verbose)
    message("Following jobnames detected: ",
            paste(jobnames, collapse = " "))
  
  njobs = length(jobnames)
  if(missing(dep_type))
    dep_type = c("none", rep("gather", njobs - 1))
  if(missing(sub_type))
    sub_type = "serial"
  if(missing(prev_jobs))
    prev_jobs = c("none", jobnames[-njobs])
  
  
  def <- data.frame(jobname = jobnames,
                    sub_type = sub_type,
                    prev_jobs = prev_jobs,
                    dep_type = dep_type,
                    queue = queue,
                    memory_reserved = memory_reserved,
                    walltime = walltime,
                    cpu_reserved = cpu_reserved,
                    nodes = nodes,
                    platform = platform,
                    stringsAsFactors = FALSE)
  
  def = as.flowdef(def, verbose = verbose)
  
  # use sub and dep detection, if prev_jobs is given:
  if(guess){
    message("> guessing submission and dependency types, this feature is experimental...")
    def <- guess_sub_dep(x, def)
  }
  
  return(def)
}


#' @export
to_flowdef.data.frame = to_flowdef.flowmat

#' @export
to_flowdef.tbl_df = to_flowdef.flowmat


#' @rdname to_flowdef
#' @export
to_flowdef.flow <- function(x, ...){
  
  slts = c(jobname = "name",
           prev_jobs = 'previous_job',
           dep_type = "dependency_type",
           sub_type = "submission_type",
           queue = "queue",
           memory_reserved = "memory",
           walltime = "walltime",
           nodes = "nodes",
           cpu_reserved = "cpu",
           status = "status",
           platform = "platform")
  
  tmp <- lapply(x@jobs, function(y){
    y = slots_as_list(y)[slts]
    y$previous_job = paste(y$previous_job, collapse = ",")
    unlist(y)
  })
  
  def = data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
  
  colnames(def) = names(slts)
  
  #kable(def)
  def = as.flowdef(def, ...)
  return(def)
}


#' @rdname to_flowdef
#' @importFrom utils write.table
#' @export
to_flowdef.character <- function(x, ...){
  
  if(!missing(x)){
    mat <- read_sheet(x)
    mat = to_flowmat(mat)
  }
  def = to_flowdef(mat)
  write.table(def, file = file.path(dirname(x), "flowdef.txt"),
              sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(def)
}


#' @rdname to_flowdef
#' @export
as.flowdef <- function(x, ...){
  
  # if its already a flowdef
  if(is.flowdef(x))
    return(check(x, ...))
  
  # if its a data.frame
  if(is.data.frame(x))
    y = x
  
  # if its a file
  if(is.character(x)){
    if(!file.exists(x))
      stop(paste0(error("no.def"), x))
    message("def seems to be a file, reading it...")
    y <- read_sheet(x, id_column = "jobname")
  }
  
  y$jobid <- 1:nrow(y)
  class(y) <- c("flowdef", "data.frame")
  
  # finally check the object
  y = check(y, ...)
  
  return(y)
}


#' @rdname to_flowdef
#' @export
is.flowdef <- function(x){
  class(x)[1] == "flowdef"
}


## needs two new functions:
## check resources
## check relationships



## -----------   this section deals with making a skeleton flowdef


## examples
if(FALSE){
  def = system.file('vignettes/ex_flow2.def', package = "ngsflows")
  
}





