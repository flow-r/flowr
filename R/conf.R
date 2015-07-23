


#' @export
.flow_opts <- new.env()

#' @rdname opts
#' @title Setting/loading and extracting various options into the environment
#' @description
#' load_conf:
#' This function extracts the options set by functions like set_opts/load_conf
#' @family conf opts
#' @aliases flow_opts ngsflows_opts get_opts set_opts print.opts
#' @param x get_opts: a character vector of names of options to extract.
#' @seealso \link{load_conf}
#' @export
#' @examples
#' ## Prints all the default options set via flowr.conf
#' get_opts()
#' get_opts("flow_run_path")
get_opts = function(x){
	if(missing(x))
		x = ls(.flow_opts)
	out = mget(x, envir = .flow_opts, ifnotfound = list(NULL))
	if(length(x) == 1){
		out = unlist(out)
	}else{
		class(out) = c("opts", "list")
	}
	return(out)
}

#debug(get_opts)

#' @rdname opts
#' @param x a named list
#' @examples
#'
#' ## set _opts
#' set_opts(list(flow_run_path = "~/mypath"))
#' @export
set_opts = function(x = list()){
	list2env(x, envir = .flow_opts)
	invisible()
}


#' @export
print.opts <- function(x){
	if(length(x) > 1){
		message("\nPrinting list of options as a pretty table.")
		x = cbind(lapply(x, function(f) {
			as.data.frame(Filter(Negate(is.null), f))
		}))
		print(kable(x))
		# 		print(kable(t(as.data.frame(x, row.names = names(x)))))
	}
	else(print.default(x))
}


.load_conf <- function(x, chk, ...){
	if(!file.exists(x)){
		message(error("no.conf"), x)
		return()
	}
	conf <- read_sheet(x, allowEscape = TRUE)
	lst = as.list(conf$value)
	names(lst) = conf$name

	lst = parse_conf(lst)

	## -- check the ones with file paths
	if(chk)
		chk_conf(lst)
	options(lst)
	set_opts(lst)
	#opts()$set(lst)
	## -- populate these in the global environment
	invisible(lst)
}

#' @title
#' Fetching and loading configuration files.
#' @description
#' These are two column tab seperated files (namely name and value).
#' @description load a configuration file into the environment
#' @param x path to a configuration file
#' @param chk check the file after loading?
#' @param ... Not used
#' @seealso \link{get_opts}
#' @export
load_conf <- function(x, chk = TRUE, ...){
	## .load_conf: works on a single file
	lst <- lapply(x, .load_conf, chk = chk, ...)

	## in future this could be a normalized list
	## with the final set of options used
	invisible(lst)
}

## process conf line by line
## use whisker to evaluate the string, given available data

#' parse_conf
#' @param lst a list of configuration options to parse
#'
#'
#' @import whisker
parse_conf <- function(lst){
	## --- sequentially evaluae each configuration
	for(i in 1:length(lst)){
		lst[[i]] = whisker.render(lst[[i]], lst)
	}
	return(lst)
}


chk_conf <- function(x){
	path_pattern = c("path$|dir$|exe$")

	pths = grep(path_pattern, names(x))

	mis_pths = !file.exists(as.character(x)[pths])

	if(sum(mis_pths) > 0){
		msg = "\n\nSeems like these paths do not exist, this may cause issues later:\n"
		df = data.frame(name = names(x)[mis_pths],
										value = as.character(x)[mis_pths])
		warning(msg, paste(kable(df, row.names = FALSE), collapse = "\n"))
	}
}



#' A generic functions to search for files
#' @description
#' These functions help in searching for specific files in the user space.
#'
#' MYRLIB/flowr/conf folder           ## flowr/ngsflows internal default configurations
#' ~/flowr/conf                       ## flowr default home
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
#' @param urls urls to look for, works well for pipelines.
#' @param verbose be chatty?
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
#' @description
#' fetch_pipes(): Looks at: github repo: ngsflows/pipelines
#' @export
fetch_pipes <- function(x, places, urls, check = TRUE, ask){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "examples"),
			system.file(package = "ngsflows", "pipelines"),
			get_opts("flow_pipe_paths"), "~/")
	}
	if(missing(x))
		avail_pipes(places)
	r = fetch(paste0("^", x, ".R$"), places)
	def = gsub("R$", "def", r)
	conf = gsub("R$", "conf", r)
	if(check)
		if(length(r) == 0)
			stop(error("no.pipe"), x)
	return(list(pipe = r, def = def, conf = conf))
}


avail_pipes <- function(){
	urls = "https://api.github.com/repositories/19354942/contents/inst/examples?recursive=1"
	urls = "https://api.github.com/repos/sahilseth/flowr/git/trees/master?recursive=1"
	GET(url)
}

#' @rdname fetch
#' @description fetch_conf(): Searching for .conf files in various places
#' @export
fetch_conf <- function(x = "flowr.conf", places, ...){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "conf"),
			system.file(package = "ngsflows", "conf"),
			get_opts("flow_conf_path"), "~/")
	}

	x = paste0(x, "$") ## x should be a full file name
	fetch(x, places = places, ...)
}


search_conf <- function(...){
	.Deprecated("fetch_conf")
	fetch_conf(...)
}


