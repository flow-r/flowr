#' @rdname fetch
#'
#' @title  A generic functions to search for files
#'
#' @description
#' These functions help in searching for specific files in the user's space.
#'
#'
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
#' @param urls urls to look for, works well for pipelines.
#' @param verbose be chatty?
#' @param ask ask before downloading or copying, not used !
#' @param ... not used
#'
#' @export
#'
#' @examples {
#' fetch_conf("torque.sh")
#' }
fetch <- function(x, places, urls, verbose = FALSE){
	y = sapply(places, list.files, pattern = paste0(x, "$"),
		full.names = TRUE)
	y = as.character(unlist(y))
	if(verbose) message(y)

	return(y)

}

#' @rdname fetch
#'
#' @description
#'
#' fetch_pipes(): Fetches pipelines in the following places,
#' \itemize{
#' \item - available in 'pipelines' folders in flowr and ngsflows packages.
#' \item - ~/flowr/pipelines
#' \item - github repos (currently not supported)
#' }
#'
#' @param silent [fetch_pipes() only]. logical, be silent even if no such pipeline is available.
#' @param last_only [fetch_pipes only]. If multiple pipelines match the pattern, return the last one.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom utils tail
#' @importFrom knitr kable
#'
#' @export
fetch_pipes <- function(x,
												places,
												last_only = FALSE,
												urls = get_opts("flowr_pipe_urls"),
												silent = FALSE,
												ask = TRUE){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "pipelines"),
			system.file(package = "ngsflows", "pipelines"),
			get_opts("flow_pipe_paths"),
			getwd())
	}

	if(missing(x)){
		message("Please supply a name of the pipline to run, here are the options")
		x = ".*"
	}

	## in case of multiple files, use the last one
	r = fetch(paste0("^", x, ".R$"), places = places, urls = urls)
	#r = tail(r, 1)
	def = gsub("R$", "def", r)
	conf = gsub("R$", "conf", r)
	pipes = data.frame(name = file_path_sans_ext(basename(r)), def = def, conf = conf, pipe = r)

	pipe_print = pipes;
	pipe_print$def = basename(pipe_print$def)
	pipe_print$conf = basename(pipe_print$conf)
	print(kable(pipe_print))

	if(last_only){
		if(nrow(pipes) > 1)
			message("\nFound multiple pipelines with the same name, will use the last from above list")
		pipes = tail(pipes, 1)
	}

	if(!silent)
		if(length(r) == 0)
			warning(error("no.pipe"), x)
	invisible(pipes)

}


load_pipe <- function(x){
	#aln_bwa_merge
}


#' @rdname fetch
#'
#' @description fetch_conf(): Fetches configuration files in the following places,
#'
#' \itemize{
#' \item - available in 'conf' folders in flowr and ngsflows packages.
#' \item - ~/flowr/conf folder
#' }
#'
#' By default flowr loads, ~/flowr/conf/flowr.conf and ~/flowr/conf/ngsflows.conf
#' @export
fetch_conf <- function(x = "flowr.conf", places, ...){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "conf"),
			system.file(package = "ngsflows", "conf"),
			get_opts("flow_conf_path"), getwd())
	}

	x = paste0(x, "$") ## x should be a full file name
	fetch(x, places = places, ...)
}


search_conf <- function(...){
	.Deprecated("fetch_conf")
	fetch_conf(...)
}

## testing....
avail_pipes <- function(){
	urls = "https://api.github.com/repositories/19354942/contents/inst/examples?recursive=1"
	urls = "https://api.github.com/repos/sahilseth/flowr/git/trees/master?recursive=1"
}

