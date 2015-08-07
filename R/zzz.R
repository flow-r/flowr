
.onLoad <- function(lib, pkg){
	packageStartupMessage("Flowr: streamlining workflows")
}

.onAttach <- function(lib, pkg){
	#print(search())
	#print(set_opts)
	fls = fetch_conf("flowr.conf")
	suppressMessages(load_conf(fls, check = FALSE))
	#get_opts()
}


.onDeatch <- function(lib, pkg) {
	#ops <- options()
	#options(ops)
}
