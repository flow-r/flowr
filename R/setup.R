#' Setup and initialize flowr
#'
#' @description 
#' This functions creates a directory structure in user's home directory. 
#' Additionally it creates a shortcut to the \code{flowr} helper script in \code{~/bin}.
#' 
#' 
#' @param bin path to bin folder
#' @param flow_base_path The base of flowr configuration and execution folders.
#' @param flow_conf_path Flowr configuration folder, used by \link{fetch_conf}.
#' @param flow_pipe_path Folder with all pipelines, used by \link{fetch_pipes}.
#' @inheritParams to_flow
#'
#' @details Will add more to this, to identify cluster and aid in other things.
#' @export
setup <- function(bin = "~/bin", 
                  flow_base_path = opts_flow$get("flow_base_path"),
                  flow_run_path = opts_flow$get("flow_run_path"),
                  flow_conf_path = opts_flow$get("flow_conf_path"),
                  flow_pipe_path = opts_flow$get("flow_pipe_paths")){
  pkg = "flowr"
  if(!file.exists(bin)) dir.create(bin) ## create bin, if it does not exist
  script = file.path(bin, pkg)
  if(!file.exists(script)){
    message("\nAdding flowr executable to ~/bin")
    file.symlink(system.file(package = pkg, "scripts/flowr"), bin)
  }
  
  
  message("Creating a directory structure under ", flow_base_path)
  dir.create(flow_base_path, showWarnings = FALSE)
  dir.create(flow_conf_path, showWarnings = FALSE)
  dir.create(flow_run_path, showWarnings = FALSE)
  dir.create(flow_pipe_path, showWarnings = FALSE)
  
  ## fetch conf files and copy them, if they exist show warning
  confs = sapply(c("flowr","ngsflows"), function(x) fetch_conf(x, verbose = 0)[1])
  message("\n>copying default configuration files to: ", flow_conf_path, 
          ".\n>would skip if they already exist. ", 
          "\n>If you are upgrading, please check the conf files for version information:\n", 
          "https://github.com/sahilseth/flowr/tree/master/inst/conf")
  tmp2 = try(file.copy(confs, flow_conf_path, overwrite = FALSE), silent = TRUE)
  
  message("\ncopying a simple pipeline to: ", flow_pipe_path[1])
  pip = fetch_pipes("sleep_pipe", silent = TRUE)[1, ]
  tmp2 = try(file.copy(c(pip$def, pip$pipe), flow_pipe_path[1]), silent = TRUE)
  
  tmp <- c("\nConsider adding ~/bin to your PATH variable in .bashrc.",
           "\nexport PATH=$PATH:$HOME/bin",
           "\nYou may now use all R functions using 'flowr' from shell.")
  message(tmp)
}
