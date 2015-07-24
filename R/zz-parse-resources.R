get_flow_memory <- function(){
	
}

# n= number of lines to read
parse_lsf_out <- function(x, scale_time = 1/3600, n = 100){
	text <- scan(x, what = "character", sep = "\t", n = n)
	cpu_time = gsub("\\s|sec\\.", "", strsplit(grep("CPU time", text, value = TRUE), ":")[[1]][2])
	avg_mem = gsub("\\s| MB", "", strsplit(grep("Average Memory", text, value = TRUE), ":")[[1]][2])
	max_mem = gsub("\\s| MB", "", strsplit(grep("Max Memory", text, value = TRUE), ":")[[1]][2])
	max_swap = gsub("\\s| MB", "", strsplit(grep("Max Swap", text, value = TRUE), ":")[[1]][2])
	return(list(cpu_time = as.numeric(cpu_time) * scale_time, avg_mem = avg_mem, max_mem = max_mem, max_swap = max_swap))
}

#' @title get_resources
#' @description get_resources currenty this only works on LSF
#' @param x A character vector of lenth 1. This may be a parent level folder with directories with multiple flow runs.
#' @param odir Output directory to save the results
#' @param \dots other arguments sent to \link{get_resources_lsf}
#' @details If \code{x} is a parent level folder, then resources are summarized for all its child folders.
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

#' @title get_resources_lsf
#' @description get_resources_lsf
#' @inheritParams get_resources
#' @param wd Path to a flow working directory
#' @param cores Number of cores to use. [Numeric]
#' @param pattern Pattern to use to get lsf stdout files. Defaults to \code{out$}
#' @importFrom tools file_path_sans_ext
#' @keywords internal
#' @examples \dontrun{
#' get_resources_lsf(wd = wd, cores = 4, pattern = out\$)
#' }
get_resources_lsf <- function(wd, cores = 4, pattern = "out$"){
	
	if (!requireNamespace("reshape2", quietly = TRUE)) {
		stop("reshape2 needed for this function to work. Please install it.",
			call. = FALSE)
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("ggplot2 needed for this function to work. Please install it.",
			call. = FALSE)
	}

	flow_mat = read_flow_detail_fl(wd)
	rownames(flow_mat) = flow_mat$jobid
	#files_cmd <- list.files(wd, pattern = "sh$", full.names = TRUE, recursive = TRUE)
	files_out <- list.files(wd, pattern = pattern, full.names = TRUE, recursive = TRUE)
	jobid = file_path_sans_ext(basename(files_out))
	names(files_out) = jobid
	#     mat_cmd <- data.frame(do.call(rbind,
	#                                   strsplit(gsub(".*/(.*)/.*/(.*)\\.([0-9]*)\\.([0-9]*)\\.output",
	#                                                 "\\1,\\2,\\3", files_out), split = ",")),
	#                           stringsAsFactors = FALSE)
	#colnames(mat_cmd) = c('oprefix', 'jobname', 'num')
	#mat_cmd <- cbind(mat_cmd, outfile = files_out)
	flow_mat$outfile = files_out[as.c(flow_mat$jobid)]
	tmp = mclapply(as.c(flow_mat$outfile), function(i) try(parse_lsf_out(i)), mc.cores = cores)
	resources <- do.call(rbind, tmp)
	mat_res <- cbind(flow_mat, resources);dim(mat_res)
	## restructure for plotting:
	mat_res$avg_mem = as.numeric(mat_res$avg_mem)
	mat_res$max_mem = as.numeric(mat_res$max_mem)
	mat_res$max_swap = as.numeric(mat_res$max_swap)
	mat_res$cpu = as.numeric(mat_res$cpu)
	mytheme <- theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
	dat = melt(mat_res, measure.vars = c("avg_mem", "max_mem", "max_swap", "cpu"))
	p <- with(dat, {ggplot(dat, aes(x = jobname, y = value)) + geom_boxplot() + geom_jitter(col = "grey", alpha = 0.3) + mytheme})
	p <- p + facet_wrap(~variable, scales = "free_y")
	ggsave(sprintf("%s/resources_utilized.pdf", wd), p)
}
