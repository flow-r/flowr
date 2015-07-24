

flow_opts = new.env()

#' @importFrom params get_opts
get_opts <- function(x){
	params::get_opts(x, flow_opts)
}

#' @importFrom params set_opts
set_opts <- function(x){
	params::set_opts(x, flow_opts)
}
