.onAttach <- function(lib, pkg){
	packageStartupMessage("Flowr: streamlining workflows")
}

.onDeatch <- function(lib, pkg) {
	ops <- options()
	#ops <- ops[grep("SaturnV_", names(ops))]
	options(ops)
}




if(FALSE){ ## need to find a smarter way to deal with this. i.e. check if PATH variable has flowr
	#packageStartupMessage("Try flowr from terminal, does not work? try...")
	if(!file.exists(system("source ~/.bashrc;which flowr", intern = TRUE))){
		c("If you have already added this and still see this message, run 'flowr' from shell and see if it works.",
			"You may also add #!/bin/bash as the first line in your .bashrc")
	}
}