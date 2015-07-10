.onAttach <- function(lib, pkg){
	packageStartupMessage("Flowr: streamlining workflows")
	fls = search_conf("flowr.conf")
	suppressMessages(load_conf(fls, chk = FALSE))
}

.onDeatch <- function(lib, pkg) {
	ops <- options()
	#ops <- ops[grep("SaturnV_", names(ops))]
	options(ops)
}

