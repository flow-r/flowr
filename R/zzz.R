.onAttach <- function(lib, pkg){
  packageStartupMessage("Flowr: streamlining workflows")
  path = "~/bin";script=file.path(path, pkg)
  if(!file.exists(path)) dir.create(path) ## create bin, if it does not exist
  if(!file.exists(script)){
    packageStartupMessage("Adding flowr executable to ~/bin")
    file.symlink(system.file(package = pkg, "scripts/flowr"), "~/bin/flowr")
  }
  #packageStartupMessage("Try flowr from terminal, does not work? try...")
  tmp <- c("Consider adding ~/bin to your PATH variable in bashrc.",
  "This would streamline invoking flowr commands from the shell.",
  "\nexport PATH=$PATH:$HOME/bin")
  if(FALSE){ ## need to find a smarter way to deal with this. i.e. check if PATH variable has flowr
    if(!file.exists(system("source ~/.bashrc;which flowr", intern = TRUE))){
      c("If you have already added this and still see this message, run 'flowr' from shell and see if it works.",
      "You may also add #!/bin/bash as the first line in your .bashrc")
    }
  }
}

.onDeatch <- function(lib, pkg) {
  ops <- options()
  ops <- ops[grep("SaturnV_", names(ops))]
  options(ops)
}
