.onAttach <- function(lib, pkg){
	packageStartupMessage("Flowr: streamlining workflows")
}

.onDeatch <- function(lib, pkg) {
	ops <- options()
	#ops <- ops[grep("SaturnV_", names(ops))]
	options(ops)
}

