

#' @rdname opts_flow
#' @format \code{opts_flow}
#' @export
flowopts = new.env()

#' @rdname opts_flow
#' 
#' @aliases set_opts get_opts load_opts
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
#' For more details regarding these funtions refer to \link{params} package.
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
#' ## Set options: set_opts()
#' opts = set_opts(flow_run_path = "~/mypath")
#' ## OR if you would like to supply a long list of options:
#' opts = set_opts(.dots = list(flow_run_path = "~/mypath"))
#'
#' ## load options from a configuration file: load_opts()
#' conffile = fetch_conf("flowr.conf")
#' load_opts(conffile)
#'
#' ## Fetch options: get_opts()
#' get_opts("flow_run_path")
#' get_opts()
#'
#' @export
#' @importFrom params new_opts

opts_flow = new_opts(flowopts)


#' @aliases params
#' @export
get_opts <- function(...){
	opts_flow$get(...)
}

#' @aliases params
#' @export
set_opts <- function(...){
	opts_flow$set(...)
}

#' @aliases params
#' @export
load_opts <- function(...){
	opts_flow$load(...)
	
	## certain opts need to be numeric
	set_opts(verbose = as.numeric(get_opts("verbose")))
	
}

#devtools::install("~/Dropbox/public/github_params")
