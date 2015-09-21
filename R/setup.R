#' Setup and initialize some scripts.
#'
#' @param bin path to bin folder
#' @param flow_base_path The base of flowr configuration and execution folders.
#' @param flow_conf_path Flowr configuration folder
#' @inheritParams to_flow
#'
#' @details Will add more to this to identify cluster and aid in other things
#' @export
setup <- function(bin = "~/bin", 
									flow_base_path = get_opts("flow_base_path"),
									flow_run_path = get_opts("flow_run_path"),
									flow_conf_path = get_opts("flow_conf_path")){
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
	dir.create(flow_conf_path, showWarnings = FALSE)
	dir.create(flow_run_path, showWarnings = FALSE)
	message(tmp)
}
