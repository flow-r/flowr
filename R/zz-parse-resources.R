# nocov start




opts_flow$set(time_format = "%a %b %e %H:%M:%S CDT %Y")

# parse LSF output files
# @param x file
# @param scale_time time is usually in seconds, scale of 1/60 shows minutes, 1/3600 shows in hours
# @param n how many lines to read; usually resources details are on top. 100 works well. .Depreciated
# @param time_format format of time in the execution logs. This should match the format in lsf/torque etc. 
# 	shell script templates.
# @param verbose produce step-by-step messages
parse_lsf_out <- function(x,
                          scale_time = 1/3600,
                          n = 100,
                          time_format = opts_flow$get("time_format"),
                          verbose = opts_flow$get('verbose')){
  
  if(verbose > 2)
    message("reading: ", x)
  
  if(!file.exists(x)){
    
    cpu_time=bgn_time=end_time=avg_mem=max_mem=max_swap=host=cores=NA
    
  }else{
    
    hd = system(paste0("head -n100 ", x), intern = TRUE)
    tl = system(paste0("tail -n100 ", x), intern = TRUE)
    text = unique(c(hd, tl))
    #text <- scan(x, what = "character", sep = "\n", quiet = TRUE)
    
    cpu_time = try(gsub("\\s|sec\\.", "", strsplit(grep("CPU time", text, value = TRUE), ":")[[1]][2]))
    cpu_time = as.numeric(cpu_time) * scale_time
    bgn_time = try(gsub("BGN at ", "", grep("^BGN at", text, value = TRUE)))
    bgn_time = try(strptime(bgn_time, format = time_format))
    end_time = try(gsub("END at ", "", grep("^END at", text, value = TRUE)))
    end_time = try(strptime(end_time, format = time_format))
    
    # get line for avg memory
    patterns = c(avg_mem = "Average Memory", 
                 max_mem = "Max Memory",
                 max_swap = "Max Swap")
    get_mem <- function(text, pattern, type){
      mem = trimws(strsplit(grep(pattern, text, value = TRUE), ":")[[1]][2])
      unit = strsplit(mem, " ")[[1]][2]
      mem = as.numeric(trimws(gsub(unit, "", mem)))
      list(mem = mem, unit = unit, type = type)
    }
    
    lst_mem = lapply(seq_along(patterns), function(i){
      get_mem(text, patterns[i], names(patterns)[i])
    })# %>% do.call(rbind, .)
    names(lst_mem) = names(patterns)
    
    host = gsub(".*host <([a-z0-9]*)>.*", "\\1", grep("host <.*>", text, value = TRUE))
    cores = gsub(".*ptile=(.*)\\].*", "\\1", grep("ptile=", text, value = TRUE))
  }
  
  #warnings()
  #message(cpu_time)
  dat = suppressWarnings(data.frame(
    cpu_time = as.numeric(cpu_time),
    bgn_time = bgn_time,
    end_time = end_time,
    avg_mem = lst_mem$avg_mem$mem,
    max_mem = lst_mem$max_mem$mem,
    max_swap = lst_mem$max_swap$mem,
    host = host,
    cores = cores, 
    stringsAsFactors = FALSE))
  
  # incase the flow was re-run, it may have multiple bgn and end times
  # we will pick the last one
  dat = tail(dat, 1)
  
  return(dat)
}

if(FALSE){
  
  # ** example -----
  x = "/rsrch3/home/iacs/sseth/flows/SS/sarco/mda/wex/ponm/runs/pon_m-20190420-00-06-46-5e1YkNUZ/pon_m-WEX-1004-N-20190420-00-06-46-7zoAKfcd/001.mutect/mutect_cmd_1.out"
  df = parse_lsf_out(x)
  class(df$bgn_time)
  
}

#' Extract resources used by each job of a flow
# 
#' get_resources currently this only works on LSF
#' @param x A character vector of length 1. This may be a parent level folder with directories with multiple flow runs.
#' @param odir Output directory to save the results
#' @param \dots other arguments sent to \link{get_resources_lsf}
#' 
#' @details If \code{x} is a parent level folder, 
#' then resources are summarized for all its child folders.
#' 
#' @export
#' 
#' @examples \dontrun{
#' get_resources(x = x, odir = ~/tmp)
#' }
get_resources <- function(x, odir, ...){
  ## Suggested packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggpplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  wds = get_wds(x)
  for(wd in wds){
    if(missing(odir)) odir = wd
    try(get_resources_lsf(wd, ...))
  }# for loop
}# function



#' get_resources_lsf
#' 
#' @inheritParams get_resources
#' @param wd Path to a flow working directory
#' @param cores Number of cores to use. [Numeric]
#' @param pattern Pattern to use to get lsf stdout files. Defaults to \code{out$}
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel mclapply
#' 
#' @keywords internal
#' 
#' @export
#' 
#' @importFrom readr write_rds write_tsv
#' 
#' @examples \dontrun{
#' get_resources_lsf(wd = wd, cores = 4, pattern = out\$)
#' }
get_resources_lsf <- function(wd, 
                              cores = 4, 
                              pattern = "out$",
                              plot = FALSE,
                              verbose = opts_flow$get("verbose")){
  
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  #fobj = read_fobj(wd)
  if(verbose)
    message("working on: ", wd)
  flowdet = to_flowdet(wd)
  flowdet$out = gsub("sh$", "out", flowdet$cmd)
  
  ## create new out files in case logs have moved.
  flowdet$out2 = file.path(wd, basename(dirname(flowdet$cmd)), 
                           gsub("sh$", "out", basename(flowdet$cmd)))
  
  to = nrow(flowdet)
  pb <- txtProgressBar(min = 1, max = to, style = 3)
  tmp = lapply( 1:to, function(i) {
    pb$up(i)
    x = as.c(flowdet$out2)[i]
    try(parse_lsf_out(x))
  })
  close(pb)
  
  resources <- do.call(rbind, tmp)
  mat_res <- cbind(flowdet, resources);dim(mat_res)
  ## restructure for plotting:
  #mat_res$avg_mem = as.numeric(mat_res$avg_mem)
  #mat_res$max_mem = as.numeric(mat_res$max_mem)
  #mat_res$max_swap = as.numeric(mat_res$max_swap)
  #mat_res$cpu_time = mat_res$cpu_time
  #mat_res$bgn_time = unlist(mat_res$bgn_time)
  #mat_res$end_time = unlist(mat_res$end_time)
  mat_res$wd = basename(wd)
  #mat_res$node = 
  
  dat = reshape2::melt(mat_res,
                       measure.vars = c("avg_mem", "max_mem", "max_swap", "cpu_time", "bgn_time", "end_time"))
  #dat$cpu_time = as.numeric(dat$cpu_time)
  if(plot){
    pacman::p_load("cowplot")
    #mytheme <- ggplot2::theme_bw() +
    p <- with(dat, {ggplot2::ggplot(dat, ggplot2::aes(x = jobname, y = value)) +
        ggplot2::geom_boxplot() + 
        ggplot2::geom_jitter(col = "grey", alpha = 0.3)})
    p <- p + ggplot2::facet_wrap(~variable, scales = "free_y")
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    cowplot::save_plot(sprintf("%s/resources_utilized.pdf", wd), p, base_width = 12, base_height = 8)
  }
  readr::write_rds(mat_res, file.path(wd, "flow_resources_wd.rds"))
  readr::write_rds(dat, file.path(wd, "flow_resources.rds"))
  readr::write_tsv(dat, file.path(wd, "flow_resources.tsv"))
  
  invisible(mat_res)
}


if(FALSE){
  # ** example ------
  
  reshape2::dcast(df, jobnm+job_no+job_sub_id+job_id ~ variable, value.var = "value")
}

# nocov end
