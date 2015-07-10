

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

## process conf line by line
## use whisker to evaluate the string, given available data

#' parse_conf 
#' @import whisker
parse_conf <- function(lst){
	## --- sequentially evaluae each configuration
	for(i in 1:length(lst)){
		lst[[i]] = whisker.render(lst[[i]], lst)
	}
	return(lst)
}


#' load a configuration file into the environment
#' x path to a configuration file
#' @export
load_conf <- function(x, chk = TRUE, ...){
	## .load_conf: works on a single file
	lst <- lapply(x, .load_conf, chk = chk, ...)
	
	## in future this could be a normalized list
	## with the final set of options used
	invisible(lst)
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


#' Given a name of a configuration file, searches for it in few places
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
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
search_conf <- function(x = "flowr.conf", places, verbose = FALSE){
	if(missing(places)){
		places = c(system.file(package = "flowr", "conf"),
			getOption("flow_conf_path"), "~/")
	}
	
	y = sapply(places, list.files, pattern = paste0(x, "$"), full.names = TRUE)
	y = as.character(unlist(y))
	if(verbose) message(y)
	
	return(y)
}

