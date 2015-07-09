

#' load a configuration file into the environment
load_conf <- function(x){
	conf <- read_sheet(x)
	
	## -- check the ones with file paths
	chk_conf(conf)
	
	lst = as.list(conf$value)
	names(lst) = conf$name
	
	## -- populate these in the global environment
	options(lst)
	invisible(lst)
}

chk_conf <- function(x){
	path_pattern = c("path$|dir$|exe$")
	
	pths = grep(path_pattern, x$name)
	
	mis_pths = !file.exists(conf$value[pths])
	
	if(sum(mis_pths) > 0){
		msg = "\n\nSeems like these paths do not exist, this may cause issues later:\n"
		warning(msg, paste(kable(x[mis_pths, ], row.names = FALSE), collapse = "\n"))
	}
}


#' Given a name of a configuration file, searches for it in few places
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
#' @details  
#' 
#' sequence: the later overrides former if parameter name matches
#' ~/                                 ## home dir
#' ~/flowr/conf                       ## flowr default home
#' MYRLIB/flowr/conf folder           ## flow pipeline folder
search_conf <- function(x = "flowr.conf", places){
	if(missing(places)){
		places = c(system.file(package = "flowr", "conf"),
			"~/flowr/conf", "~/")
	}
	
	y = list.files(path = places, pattern = x, full.names = TRUE)
	
	return(y)
}

