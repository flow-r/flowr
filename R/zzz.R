
.onLoad <- function(lib, pkg){

}

.onAttach <- function(lib, pkg){
	#print(search())
	#print(set_opts)
	packageStartupMessage("Flowr: streamlining workflows")
	
	fls = unique(unlist(sapply(c("flowr"), fetch_conf)))
	suppressMessages(opts_flow$load(fls, check = FALSE))
	
	if(opts_flow$get('verbose') > 1)
		packageStartupMessage("\nverbose level: 2 (debug mode)\nfollowing files are being loaded:\n", paste(fls, "\n"))

}


.onDeatch <- function(lib, pkg) {
	#ops <- options()
	#options(ops)
}
