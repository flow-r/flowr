

#' @rdname opts_flow
#' @format \code{opts_flow}
#' @export
flowopts = new.env()

#' @rdname opts_flow
#' @title Default options/params used in ngsflows and flowr
#'
#' @description
#' There are three helper functions which attempt to manage params used by flowr and ngsflows: 
#' \itemize{
#' \item \link{get_opts} OR \code{opts_flow$get}: show all default options
#' \item \link{set_opts} OR \code{opts_flow$set}: set default options
#' \item \link{load_opts} OR \code{opts_flow$load}: load options specified in a tab seperated text file
#' }
#' For more details regarding these funtions refer to \link{params}.
#' 
#' @param ... \itemize{
#' \item get: names of options to fetch
#' \item set: a set of options in a name=value format seperated by commas
#' }
#' 
#' @details
#' By default flowr loads, \code{~/flowr/conf/flowr.conf} and \code{~/flowr/conf/ngsflows.conf}
#'
#' Below is a list of default flowr options, retrieved via 
#' 
#' \code{opts_flow$get()}:
#'
#' \Sexpr[results=verbatim]{flowr::opts_flow$get()}
#' 
#' @examples 
#' ## Set options: set_opts()
#' opts = set_opts(flow_run_path = "~/mypath")
#' ## OR if you would like to supply a long list of options:
#' opts = set_opts(.dots = list(flow_run_path = "~/mypath"))
#' 
#' ## load options from a configuration file: load_opts()
#' myconfile = fetch_conf("flowr.conf")
#' load_opts(myconfile)
#' 
#' ## Fetch options: get_opts()
#' get_opts("flow_run_path")
#' get_opts()
#'
#' @export
#' @importFrom params new_opts
opts_flow = new_opts(flowopts)



#' @rdname opts_flow
#' @export
get_opts <- function(...){
	opts_flow$get(...)
}

#' @rdname opts_flow
#' @export
set_opts <- function(...){
	opts_flow$set(...)
}

#' @rdname opts_flow
#' @export
load_opts <- function(...){
	opts_flow$load(...)
}

#devtools::install("~/Dropbox/public/github_params")
