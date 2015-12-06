

#' @rdname flowopts
#' @format \code{opts_flow}
#' @export
flowopts = new.env()

#' @rdname flowopts
#' 
#' @aliases set_opts get_opts load_opts opts_flow$set opts_flow$get opts_flow$load
#' 
#' @title Default options/params used in flowr and ngsflows
#'
#' @description
#' There are three helper functions which attempt to manage parameters used by flowr and ngsflows:
#' \itemize{
#' \item \link[params]{get_opts} OR \code{opts_flow\$get()}: show all default options
#' \item \link[params]{set_opts} OR \code{opts_flow\$set()}: set default options
#' \item \link[params]{load_opts} OR \code{opts_flow\$load()}: load options specified in a tab seperated text file
#' }
#' For more details regarding these funtions refer to \href{http://sahilseth.com/params}{params} package.
#'
#' @param ... \itemize{
#' \item get: names of options to fetch
#' \item set: a set of options in a name=value format seperated by commas
#' }
#'
#' @usage 
#' 
#' get_opts(...)
#' set_opts(...)
#' load_opts(...)
#' 
#' @details
#' By default flowr loads, \code{~/flowr/conf/flowr.conf} and \code{~/flowr/conf/ngsflows.conf}
#'
#' Below is a list of default flowr options, retrieved via
#'
#' \code{opts_flow$get()}:
#' \preformatted{
#'	|name              |value                    |
#'	|:-----------------|:------------------------|
#'	|default_regex     |(.*)                     |
#'	|flow_base_path    |~/flowr                  |
#'	|flow_conf_path    |~/flowr/conf             |
#'	|flow_parse_lsf    |.*(\<[0-9]*\>).*         |
#'	|flow_parse_moab   |(.*)                     |
#'	|flow_parse_sge    |(.*)                     |
#'	|flow_parse_slurm  |(.*)                     |
#'	|flow_parse_torque |(.?)\..*                 |
#'	|flow_pipe_paths   |~/flowr/pipelines        |
#'	|flow_pipe_urls    |~/flowr/pipelines        |
#'	|flow_platform     |local                    |
#'	|flow_run_path     |~/flowr/runs             |
#'	|my_conf_path      |~/flowr/conf             |
#'	|my_dir            |path/to/a/folder         |
#'	|my_path           |~/flowr                  |
#'	|my_tool_exe       |/usr/bin/ls              |
#'	|time_format       |\%a \%b \%e \%H:\%M:\%S CDT \%Y |
#'	|verbose           |FALSE                    |
#'	}
#'
#' @seealso \link{fetch} \link[params]{params} \link[params]{read_sheet}
#' 
#' @examples
#' ## Set options: opts_flow$set()
#' opts = opts_flow$set(flow_run_path = "~/mypath")
#' ## OR if you would like to supply a long list of options:
#' opts = opts_flow$set(.dots = list(flow_run_path = "~/mypath"))
#'
#' ## load options from a configuration file: opts_flow$load()
#' conffile = fetch_conf("flowr.conf")
#' opts_flow$load(conffile)
#'
#' ## Fetch options: get_opts()
#' opts_flow$get("flow_run_path")
#' opts_flow$get()
#'
#' @export
#' @importFrom params new_opts
opts_flow = new_opts(flowopts)


#' @aliases params
#' @export
get_opts <- function(...){
  message("Its better to use: opts_flow$get instead")
	opts_flow$get(...)
}

#' @aliases params
#' @export
set_opts <- function(...){
  message("Its better to use: opts_flow$set instead")
  opts_flow$set(...)
}

# redefine opts_flow$load

opts_flow$load <- function(...){
  params::load_opts(..., envir = flowopts)
  
  # certain opts need to be numeric; force and change verbose to numeric
  opts_flow$set(verbose = as.numeric(opts_flow$get("verbose")))
  
}

#' @aliases params
#' @export
load_opts <- function(...){
  message("Its better to use: opts_flow$load instead")
  opts_flow$load(...)
  
}

#devtools::install("~/Dropbox/public/github_params")
