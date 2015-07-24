.onAttach <- function(lib, pkg){
	packageStartupMessage("Flowr: streamlining workflows")
}

.onDeatch <- function(lib, pkg) {
	ops <- options()
	#ops <- ops[grep("SaturnV_", names(ops))]
	options(ops)
}


.onLoad <- function(lib, pkg){
	fls = fetch_conf("flowr.conf")
	#suppressMessages(params::load_conf(fls, check = FALSE))
}
