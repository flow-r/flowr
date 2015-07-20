

.load_conf <- function(x, chk, ...){
	conf <- read_sheet(x, allowEscape = TRUE)
	lst = as.list(conf$value)
	names(lst) = conf$name
	
	lst = parse_conf(lst)
	
	## -- check the ones with file paths
	if(chk)
		chk_conf(lst)

		## -- populate these in the global environment
	options(lst)
	invisible(lst)
	
}


#' load a configuration file into the environment
#' @param x path to a configuration file
#' @param chk check the file after loading?
#' @param ... Not used
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
#' 
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
#' @param urls urls to look for, works well for pipelines.
#' @param verbose be chatty?
#' 
#' @export
#' @details  
#' 
#' sequence: the later overrides former if parameter name matches
#' ~/                                 ## home dir
#' ~/flowr/conf                       ## flowr default home
#' MYRLIB/flowr/conf folder           ## flow pipeline folder
#' @examples {
#' search_conf("torque.sh")
#' }
fetch <- function(x, pattern, places, urls, verbose = FALSE){
	y = sapply(places, list.files, pattern = paste0(x, "$"),
						 full.names = TRUE)
	y = as.character(unlist(y))
	if(verbose) message(y)
	
	return(y)
	
}

#' @rdname fetch
#' @details 
#' Looks at: github repo: ngsflows/pipelines 
#' @export
fetch_pipes <- function(places, urls){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "examples"),
			system.file(package = "ngsflows", "pipelines"),
			getOption("flow_pipe_path"), "~/")
	}
	
}

#' @rdname fetch
#' @export
fetch_conf <- function(x = "flowr.conf", places, ...){
	if(missing(places)){
		places = c(
			system.file(package = "flowr", "conf"),
			system.file(package = "ngsflows", "conf"),
			getOption("flow_conf_path"), "~/")
	}
	
	x = paste0(x, "$") ## x should be a full file name
	fetch(x, places = places, ...)
}


search_conf <- function(...){
	.Deprecated("fetch_conf")
	fetch_conf(...)
}


