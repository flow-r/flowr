#' Setup and initialize some scripts.
#' @details Will add more to this to identify cluster and aid in other things
#' @export
setup <- function(bin = "~/bin", pkg = "flowr"){
	if(!file.exists(bin)) dir.create(bin) ## create bin, if it does not exist
	script = file.path(bin, pkg)
	if(!file.exists(script)){
		message("Adding flowr executable to ~/bin")
		file.symlink(system.file(package = pkg, "scripts/flowr"), bin)
	}
	tmp <- c("Consider adding ~/bin to your PATH variable in .bashrc.",
		"\nexport PATH=$PATH:$HOME/bin")
	message(tmp)
}