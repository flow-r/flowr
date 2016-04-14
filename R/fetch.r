#' @rdname fetch
#'
#' @title  Two generic functions to search for pipelines and configuration files.
#'
#' @description
#' These functions help in searching for specific files in the user's space.
#'
#' \code{fetch_pipes()}: Fetches pipelines in the following places, in this specific order:
#' \itemize{
#' \item \strong{user's folder}: \code{~/flowr/pipelines}
#' \item \strong{current wd}: \code{./}
#' }
#' 
#' \strong{NOTE:} If same pipeline is availabe in multiple places; intitutively, one from the later
#' folder would be selected. As such, giving priority to user's home, and current working 
#' directories.
#' 
#' <br>
#' \code{fetch_conf()}: Fetches configuration files in ALL of the following places:
#' \itemize{
#' \item \strong{package}: \code{conf} folders in flowr and ngsflows packages.
#' \item \strong{user's folder}: \code{~/flowr/conf} folder.
#' \item \strong{current wd}: \code{./}
#' }
#' 
#' \strong{NOTE:}
#' This function would greedily return all matching conf files. One would load all of them 
#' in the order returned by this function. If the same variable is
#' repeated in multiple files, value from later files would replace those formerly defined.
#' Thus ( as explained above ), giving priority to options defined in user's home and current working directories.
#' 
#' By default flowr loads, \code{flowr.conf} and \code{ngsflows.conf}. 
#' See the details sections, for more explanation on this.
#' 
#' 
#'
#' @param x name of the file to search for (without extension). 
#' By default \link{fetch_pipes} and \link{fetch_conf} search for files ending with 
#' \code{.R} and \code{.conf} respectively.
#' @param places places (paths) to look for files matching the name. Defaults are already defined in the function.
#' 
#' @param silent fetch_pipes(): logical, be silent even if no such pipeline is available. [FALSE]
#' @param last_only fetch_pipes():. If multiple pipelines match the pattern, return the last one. [TRUE]
#' @param urls urls to look for, works well for pipelines [not implemented yet]
#' @param ask ask before downloading or copying. [not implemented]
#' @param ... [not implemented]
#' 
#' @inheritParams to_flow
#'
#' @details 
#' 
#' For example flowr has a variable \code{flow_run_path} where it puts all the execution logs etc.
#' The default value is picked up from the internal \strong{flowr.conf} file.
#' To redefine this value, one could create a new file called \strong{~/flowr/conf/flowr.conf} and 
#' add a line:
#' 
#' \code{flow_run_path TAB my_awesome_path}, where \code{TAB} is a tab character, since these are tab 
#' seperated files.
#' 
#' Also, at any time you can run, \link{opts_flow$load}; to load custom options.
#' 
#' @importFrom tools file_path_sans_ext
#' @importFrom utils tail
#' @importFrom params kable
#' @export
#' @seealso \link{flowopts}
#'
#' @examples
#' 
#' ## let us find a default conf file
#' conf = fetch_conf("flowr.conf");conf
#' ## load this
#' opts_flow$load(conf)
#' 
#' ## this returns a list, which prints pretty
#' pip = fetch_pipes("sleep_pipe")
#' pip$name
#' pip$pipe
#' pip$def
#' 
fetch <- function(x, places, urls, 
                  verbose = opts_flow$get("verbose")){
  if(is.null(verbose))
    verbose = FALSE
  y = sapply(places, list.files, pattern = paste0(x, "$"),
             full.names = TRUE)
  y = as.character(unlist(y))
  if(verbose > 1) 
    message(y)
  return(y)
}

#' @rdname fetch
#'
#' @export
fetch_pipes <- function(x,
                        places,
                        last_only = FALSE,
                        urls = opts_flow$get("flowr_pipe_urls"),
                        silent = FALSE,
                        verbose = opts_flow$get("verbose"),
                        ask = TRUE){
  
  if(missing(places)){
    places = c(
      system.file(package = "flowr", "pipelines"),
      system.file(package = "flowr", "inst/pipelines"),
      system.file(package = "ngsflows", "pipelines"),
      system.file(package = "ngsflows", "inst/pipelines"),
      strsplit(opts_flow$get("flow_pipe_paths"), ",")[[1]],
      getwd())
    
    # remove missing paths
    places = places[!places == ""]
  }
  
  if(verbose)
    message("> searching for pipes in the following places: \n  ", paste(na.omit(places), collapse = "\n  "), "\n")
  
  if(missing(x)){
    message("> since no search pattern was supplied, here is the complete list of available pipelines:")
            # "You need to specify the name of the pipeline to run, like this:\n")
            # "\nflowr run x=sleep_pipe\n")
    x = ".*"
  }
  
  ext = tools::file_ext(x)
  if(!ext == ""){
    # removing extension
    x = gsub(paste0(ext, "$"), "", x)
    warning("> It is best to supply only the name of the pipeline, without the extension. ", 
            "We add a .R extention before searching. Also, this name also corresponds, ",
            "to the R function.")
  }else{
    ext = ".R" # default extension of all pipelines.
  }
  
  # first check if its a full path
  fl = paste0(x, ext)
  if(file.exists(fl))
    r = fl
  else
    r = fetch(paste0("^", x, ext, "$"), places = places, urls = urls, verbose = FALSE)
  
  if(length(r) == 0){
    #message("> no pipeline found. Downloading from github, not implemented yet.")
  }
  
  ## seemed travis was repeating some of them
  ## seen here: http://docs.flowr.space/en/latest/rd/vignettes/build-pipes.html#available-pipelines
  r = unique(r)
  
  #r = tail(r, 1)
  def = gsub("R$", "def", r)
  def = ifelse(file.exists(def), def, NA)
  
  conf = gsub("R$", "conf", r)
  conf = ifelse(file.exists(conf), conf, NA)
  
  
  pipes = data.frame(name = file_path_sans_ext(basename(r)), 
                     def = def, 
                     conf = conf, 
                     pipe = r, stringsAsFactors = FALSE)
  
  # for a pipeline, def is ESSENTIAL
  pipes = subset(pipes, !is.na(def))

  # no pipe!
  # if(verbose > 0 & !silent)
  #print(nrow(pipes))
  #print(pipes)
  if(nrow(pipes) == 0)
    stop("> could not find a pipeline called '", x, "'. Run 'flowr fetch_pipes' to see the full list.\n")
    #stop(error("no.pipe"), paste(x, collapse = "\n"))

  
  pipe_print = pipes;
  pipe_print$def = basename(as.character(pipe_print$def))
  pipe_print$conf = basename(as.character(pipe_print$conf))
  
  if(verbose > 0 & !silent) 
    message(paste(kable(pipe_print), collapse = "\n"))
  
  if(last_only){
    if(nrow(pipes) > 1 & x != ".*")
      message("> Found multiple pipelines with the same name, will use the last from above list")
    pipes = tail(pipes, 1)
  }
  
  invisible(pipes)
}


load_pipe <- function(x){
  #aln_bwa_merge
}


#' @rdname fetch
#' 
#' @export
fetch_conf <- function(x = "flowr.conf", places, ...){
  if(missing(places)){
    places = c(
      system.file(package = "flowr", "conf"),
      system.file(package = "flowr", "inst/conf"),
      # system.file(package = "ngsflows", "conf"),
      # system.file(package = "ngsflows", "inst/conf"),
      file.path(path.expand("~"), "flowr/conf/flowr.conf"),
      opts_flow$get("flow_conf_path"), 
      getwd())
  }
  
  ext = tools::file_ext(x)
  if(ext == "")
    x = paste0(x, ".conf")
  
  x = paste0(x, "$") ## x should be a full file name
  fetch(x, places = places, ...)
}


search_conf <- function(...){
  .Deprecated("fetch_conf")
  fetch_conf(...)
}

## testing....


avail_pipes <- function(){
  #urls = "https://api.github.com/repositories/19354942/contents/inst/examples?recursive=1"
  #urls = "https://api.github.com/repos/sahilseth/flowr/git/trees/master?recursive=1"
  
}

