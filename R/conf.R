

flowopts = new.env()

#' @rdname flow_opts
#' @title
#' options manager for flowr and ngsflows.
#' @export
#' @importFrom params new_opts
opts_flow = new_opts(flowopts)


## override package defaults

#' @export
get_opts <- function(...){
	opts_flow$get(...)
}

#' @export
set_opts <- function(...){
	opts_flow$set(...)
}

#' @export
load_conf <- function(...){
	opts_flow$load(...)
}

#devtools::install("~/Dropbox/public/github_params")
