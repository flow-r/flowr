

load_conf <- function(){
	params <- read.table(file,as.is=TRUE,sep="\t",strip.white=TRUE)
	for(i in 1:dim(params)[1]){
		l=list(params[i,2])
		names(l)=params[i,1]
		if(grepl("file.ngs",names(l))){
			l <- file.path(path.package(pkgname),"files",paste(params[i,2]))
			names(l) <- substr(paste(params[i,1]),6,nchar(params[i,1]))
			l <- as.list(l)
		}
		options(l)
	}
	if(verbose)
		print(params)
	return(params)
}



#' Given a name of a configuration file, searches for it in few places
#' @param x name of the file to search for
#' @param places places (paths) to look for it. Its best to use the defaults
#' @details  
#' 
#' sequence: the later overrides former if parameter name matches
#' MYRLIB/flowr/conf folder
#' ~/flowr/conf
search_conf <- function(x="flowr.conf", places){
	if(missing(places)){
		places = c(system.file(package = "flowr", "conf"),
			"~/flowr/conf")
		
	}
	
}

