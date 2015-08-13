# nocov start


get_flow_memory <- function(){

}




set_opts(time_format = "%a %b %e %H:%M:%S CDT %Y")

#' parse LSF output files
#' @param x file
#' @param scale_time time is usually in seconds, scale of 1/60 shows minutes, 1/3600 shows in hours
#' @param n how many lines to read; usually resources details are on top. 100 works well. .Depreciated
#' @param time_format format of time in the execution logs. This should match the format in lsf/torque etc. shell script templates.
parse_lsf_out <- function(x,
	scale_time = 1/3600,
	n = 100,
	time_format = get_opts("time_format")){
	text <- scan(x, what = "character", sep = "\n")

	cpu_time = try(gsub("\\s|sec\\.", "", strsplit(grep("CPU time", text, value = TRUE), ":")[[1]][2]))
	bgn_time = try(gsub("BGN at ", "", grep("^BGN at", text, value = TRUE)))
	bgn_time = try(as.character(strptime(bgn_time, format = time_format)))
	end_time = try(gsub("END at ", "", grep("^END at", text, value = TRUE)))
	end_time = try(as.character(strptime(end_time, format = time_format)))

	avg_mem = try(gsub("\\s| MB", "", strsplit(grep("Average Memory", text, value = TRUE), ":")[[1]][2]))
	max_mem = try(gsub("\\s| MB", "", strsplit(grep("Max Memory", text, value = TRUE), ":")[[1]][2]))
	max_swap = try(gsub("\\s| MB", "", strsplit(grep("Max Swap", text, value = TRUE), ":")[[1]][2]))

	return(list(cpu_time = as.numeric(cpu_time) * scale_time,
		bgn_time = bgn_time,
		end_time = end_time,
		avg_mem = avg_mem,
		max_mem = max_mem,
		max_swap = max_swap))
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
#' @importFrom parallel mclapply
#' @keywords internal
#' @examples \dontrun{
#' get_resources_lsf(wd = wd, cores = 4, pattern = out\$)
#' }
get_resources_lsf <- function(wd, cores = 4, pattern = "out$", plot = FALSE){

	if (!requireNamespace("reshape2", quietly = TRUE)) {
		stop("reshape2 needed for this function to work. Please install it.",
			call. = FALSE)
	}
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("ggplot2 needed for this function to work. Please install it.",
			call. = FALSE)
	}

	#fobj = read_fobj(wd)
	flowdet = to_flowdet(wd)
	flowdet$out = gsub("sh$", "out", flowdet$cmd)

	tmp = mclapply(as.c(flowdet$out), function(x) try(parse_lsf_out(x)), mc.cores = cores)
	resources <- do.call(rbind, tmp)
	mat_res <- cbind(flowdet, resources);dim(mat_res)
	## restructure for plotting:
	mat_res$avg_mem = as.numeric(mat_res$avg_mem)
	mat_res$max_mem = as.numeric(mat_res$max_mem)
	mat_res$max_swap = as.numeric(mat_res$max_swap)
	mat_res$cpu = as.numeric(mat_res$cpu)
	mat_res$bgn_time = unlist(mat_res$bgn_time)
	mat_res$end_time = unlist(mat_res$end_time)
	mat_res$wd = basename(wd)

	dat = reshape2::melt(mat_res,
						 measure.vars = c("avg_mem", "max_mem", "max_swap", "cpu", "bgn_time", "end_time"))
	if(plot){
		mytheme <- ggplot2::theme_bw() +
			ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
		p <- with(dat, {ggplot2::ggplot(dat, ggplot2::aes(x = jobname, y = value)) +
				ggplot2::geom_boxplot() + ggplot2::geom_jitter(col = "grey", alpha = 0.3) + mytheme})
		p <- p + ggplot2::facet_wrap(~variable, scales = "free_y")
		ggplot2::ggsave(sprintf("%s/resources_utilized.pdf", wd), p)
	}
	return(dat)
}


# nocov end
