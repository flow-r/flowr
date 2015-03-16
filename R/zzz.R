.onAttach <- function(lib, pkg){
  packageStartupMessage("Flow: streamlining workflows")
  file.link(system.file(package = pkg, "scripts/flowr"), "~/bin/")
}

.onDeatch <- function(lib, pkg) {
    ops <- options()
    ops <- ops[grep("SaturnV_", names(ops))]
    options(ops)
}
