#' Setup and initialize some scripts.
#'
#' @param bin path to bin folder
#' @param flow_base_path the root folder for all flowr operations
#'
#' @details Will add more to this to identify cluster and aid in other things
#' @export
setup <- function(bin = "~/bin", flow_base_path = get_opts("flow_base_path")){
	pkg = "flowr"
	if(!file.exists(bin)) dir.create(bin) ## create bin, if it does not exist
	script = file.path(bin, pkg)
	if(!file.exists(script)){
		message("Adding flowr executable to ~/bin")
		file.symlink(system.file(package = pkg, "scripts/flowr"), bin)
	}
	tmp <- c("Consider adding ~/bin to your PATH variable in .bashrc.",
		"\nexport PATH=$PATH:$HOME/bin",
		"\nYou may now use all R functions using 'flowr' from shell.")

	message("Creating a directory structure under ", flow_base_path)
	dir.create(flow_base_path, showWarnings = FALSE)
	dir.create(flow_base_path, showWarnings = FALSE)
	dir.create(flow_base_path, showWarnings = FALSE)
	message(tmp)
}
