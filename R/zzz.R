
.onLoad <- function(lib, pkg){
	fls = fetch_conf("flowr.conf")
	suppressMessages(load_opts(fls, check = FALSE))
}

.onAttach <- function(lib, pkg){
	#print(search())
	#print(set_opts)
	packageStartupMessage("Flowr: streamlining workflows")
	#get_opts()
}


.onDeatch <- function(lib, pkg) {
	#ops <- options()
	#options(ops)
}
