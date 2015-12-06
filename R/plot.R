

#' @rdname plot_flow
#'
#' @title 
#' Plot a clean and scalable flowchart describing the (work)flow
#' 
#' @description 
#' Plot a flowchart using a flow object or flowdef
#'
#' @aliases plot_flow plot_flow.list plot_flow.flow
#' @aliases plot
#'
#' @param x Object of class \code{flow}, or a list of flow objects or a flowdef
#' @param detailed include submission and dependency types in the plot [TRUE]
#' @param pdf create a pdf instead of plotting interactively [FALSE]
#' @param pdffile output file name for the pdf file. [\code{flow_path/flow_details.pdf}]
#' @param type 1 is original, and 2 is a elipse with less details [1]
#' @param ... experimental and only for advanced use.
#'
#' @export plot_flow
#' @import diagram
#' @examples
#' qobj = queue(type="lsf")
#' cmds = rep("sleep 5", 10)
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
#' jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter",
#'              dependency_type = "serial", previous_job = "job1")
#' fobj <- flow(jobs = list(jobj1, jobj2))
#' plot_flow(fobj)
#'
#' ### Gather: many to one relationship
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
#' jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter",
#'              dependency_type = "gather", previous_job = "job1")
#' fobj <- flow(jobs = list(jobj1, jobj2))
#' plot_flow(fobj)
#'
#' ### Burst: one to many relationship
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial", name = "job1")
#' jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter",
#'              dependency_type = "burst", previous_job = "job1")
#' fobj <- flow(jobs = list(jobj1, jobj2))
#' plot_flow(fobj)
#'
plot_flow <- function(x, ...) {
	
	#message("input x is ", class(x))
	UseMethod("plot_flow") # nocov
}

## ------------- make a flowchart using the object
#' @rdname plot_flow
#' @export
plot_flow.flow <- function(x, ...){
	

	#dat <- create_jobs_mat(x)
	x = to_flowdef(x) # nocov
	plot_flow(x, ...) # nocov
}


## compatible with a list of flows as well !

#' @rdname plot_flow
#' @export
plot_flow.list <- function(x, ...){ # nocov start
	tmp <- lapply(x, function(y)
		plot_flow(y, ...))
	invisible(tmp)
} # nocov end

#' @rdname plot_flow
#' @export
plot_flow.character <- function(x, ...){
	def = as.flowdef(x)
	plot_flow(def, ...)
}


#' @rdname plot_flow
#' @export
plot_flow.flowdef <- function(x,
															detailed = TRUE,
															type = c('1','2'),
															pdf = FALSE,
															pdffile,
															...){
	
	type = match.arg(type)
	
	## if pdffile is provide and pdf is FALSE
	if(!missing(pdffile))
		pdf = TRUE
	if(missing(pdffile) & pdf)
		pdffile = sprintf("%s.pdf", getwd())
	
	##--- plotting needs prev_jobs to be NA and not none
	x$prev_jobs = ifelse(x$prev_jobs == "none", NA, x$prev_jobs)
	p <- switch(type,
							'1' = .plot_flow_dat_type1(x=x, detailed = detailed, pdf = pdf, pdffile=pdffile, ...),
							'2' = .plot_flow_dat_type2(x=x, detailed = detailed, pdf = pdf, pdffile=pdffile, ...))
	invisible(p)
}

# split_multi_dep
# Split rows with multiple dependencies
# @param x this is a flow def
#' @importFrom utils head
split_multi_dep <- function(x){
	## --- handle cases where we have multiple dependencies
	multi_rows <- grep(",", x$prev_jobs)
	prev_col = which(colnames(x) == "prev_jobs")
	
	if (length(multi_rows)>0){
		x2 <- data.frame()
		for(i in 1:length(multi_rows)){
			## always get the current index
			row = head(grep(",", x$prev_jobs), 1)
			prev_jobs = prev_jobs = strsplit(as.c(x[row,]$prev_jobs),",")[[1]]
			dt = x[row, ] ## dt to be removed
			x2 = suppressWarnings(cbind(dt[, -prev_col], prev_jobs)) ## new df to be added
			## insert at the right place
			before = x[1:(row-1),]
			after = x[(row+1):nrow(x), ]
			if(nrow(x) == row)
				x <- rbind(before, x2)
			else
				x <- rbind(before, x2, after)
		}
	}
	return(x)
}


arrange_flowdef <- function(x, n = 4){
	jobnames=unique(as.c(x$jobname))
	
	## number time, one needs to run arrange
	n = length(jobnames) / 1.5
	jobid <- 1:length(jobnames);names(jobid)=jobnames
	prev_jobid <- jobid[as.c(x$prev_jobs)]
	get_new_ids <- function(x){
		jobnames = unique(as.c(x$jobname))
		jobid <- 1:length(jobnames);names(jobid)=jobnames
		prev_jobid <- jobid[as.c(x$prev_jobs)]
		x$jobid <- jobid[as.c(x$jobname)];
		x$prev_jobid <- prev_jobid
		return(x)
	}
	if (n == 0){
		return(get_new_ids(x))
	}
	for(j in 1:n){
		x = get_new_ids(x)
		x <- x[order(x$prev_jobid, x$jobid, na.last=FALSE, decreasing=FALSE),]
	}
	return(x)
}

display_mat <- function(x, verbose = opts_flow$get("verbose")){
	
	check_args()
	
	x$level = 0
	for(i in 1:nrow(x)){
		prev = x$prev_jobs[i]
		nm = x$jobname[i]
		
		if(verbose > 2)
			message("display_mat: index: ", i, " nm: ", nm, " prev: ", prev)
		
		if (!is.na(prev)){
			if (prev != ""){ ## if prev exists
				prev_level = subset(x, x$jobname == prev)$level
				if(verbose > 2)
					message("prev_level: ", prev_level, " x$level[i]: ", x$level[i])
				x$level[i] = prev_level + 1
			}
		}
		
	}
	table(x$level)
}

# Calculate Size of the box
# 
# Internal function (called by plot_flow), used to calculate size of box.
# 
# @param x number of jobs
# @param detailed detailed
# @param pdf pdf
#' @importFrom grDevices dev.size
calc_boxdim <- function(x, detailed, pdf){
	
	h = dev.size("cm")[2] ## height
	
	if(x > 15 & detailed)
		message("Plotting may not be pretty with big flows ",
						"you may try with, detailed=FALSE",
						"")
	
	## eq from eureka
	## smaller boxes for bigger flows
	ht = round(0.05 - 0.0012*x, 3)
	
	if(!detailed)
		ht = ht*0.9
	
	if(pdf)
		ht = ht - 0.00
	
	wd = ht * 2
	
	detail.offset = c(0, ht*0.6) ## tweak the offset a little
	list(wd = wd, ht = ht, detail.offset = detail.offset)
}

# Calculate font size based on the size of the window
# 
# Internal function (called by plot_flow), used to calculate font size.
#
# @param verbose display verbose messages
# @param x box height
#' @importFrom grDevices dev.size
calc_fontsize <- function(x, verbose = opts_flow$get("verbose")){
	
	## get height of the window
	h = dev.size("px")[2]
	
	cex =  0.3 + 0.001*h + 3*x
	cex_detail = 0.7*cex
	
	if(verbose > 1)
		message("window size: ", h, "px cex: ", cex)
	
	list(cex = cex, cex_detail = cex_detail)
}

# Uses height of the box to calculate size of the shadow
# 
# Internal function (called by plot_flow), used to calculate size of box.
#
# @param x boxht
calc_shadowsize <- function(x){
	## get height of the window
	sz = x * 0.013
	by = sz * 3
	seq(from=sz, by=by, length.out = 4)
}


# Calculate size of Arrows
# 
# Internal function (called by plot_flow), used to calculate size of arrows.
#
# @param pdf creating pdf of displaying interactively
# @param verbose display verbose messages
# @param x boxht height of the box, as returned by calc_box
calc_arrows <- function(x, pdf, verbose = opts_flow$get("verbose")){
	
	## width of the arrow is 40 times the box ht
	lwd=x*40;
	
	## length is 20% of of line width
	## the units are for diagram package
	len = lwd * 0.2;
	
	## position, where to put the arrow in the conencting lines
	pos=0.55
	
	if(pdf){
		message("plotting a pdffile...")
		lwd = 0.7 + x*60; ## need thicker
		len = 0.5 + x*20; ## need them smaller than usual
	}

	list(lwd = lwd, len = len, pos = pos)
}


#' @importFrom grDevices dev.off
#' @importFrom graphics par
#' @importFrom stats complete.cases
.plot_flow_dat_type1 <- function(x,
																 detailed = FALSE,
																 pdf = FALSE,
																 ## vector of columns to be used in plotting
																 pdffile = sprintf("flow_details.pdf"),
																 width, height, 
																 verbose = opts_flow$get("verbose"), 
																 ...){
	
	if (missing(height))  height = 2.5 * nrow(x)
	if (missing(width)) width = 2 * nrow(x)
	if (nrow(x) < 2) return(c("need a few more jobs.."))
	
	## split multiple dependencies
	x = split_multi_dep(x)
	x = arrange_flowdef(x)
	
	jobnames=unique(as.c(x$jobname))
	dat_dep <- x[complete.cases(x),] ## remove first two
	
	## Get the first row for every job
	dat_uniq <- x[sapply(jobnames, function(j) which(x$jobname==j)[1]),]
	
	## -------- get positions
	#disp_mat <- table(ifelse(is.na(dat_uniq$prev_jobid), 0, dat_uniq$prev_jobid))
	disp_mat = display_mat(dat_uniq)
	elpos <- coordinates(disp_mat)
	
	## open PDF before calculations, this IMP
	## calc use, dev.size
	if (pdf) pdf(file=pdffile, width = width, height = height)
	
	## -------- graphic params:
	shadow.col <- "lightskyblue4";
	tmp = calc_boxdim(nrow(x), detailed, pdf)
	boxwd=tmp$wd;boxht=tmp$ht;
	
	box.lcol = "gray26"
	shadow.sizes.scatter = calc_shadowsize(boxht)
	shadow.sizes.serial=shadow.sizes.scatter[1]
	
	fontsize = calc_fontsize(boxht)
	cex = fontsize$cex
	cex_detail = fontsize$cex_detail
	textcol = "gray30"
	detail.offset = tmp$detail.offset
	
	## arrows
	arr.col="gray26";
	arr = calc_arrows(boxht, pdf = pdf)
	arr.lwd = arr$lwd
	arr.len = arr$len
	arr.pos = arr$pos
	curves=c(-0.2,0.2);
	
	## final params:
	if(verbose > 1) 
		message("font size: ", cex, " ", cex_detail, 
						"\nbox size: H X W ", boxht, " X ", boxwd,
						"\narr size: lwd, len, pos: ", arr.lwd, " ", arr.len, " ", arr.pos)
	
	
	#detailed.labs = sprintf("%s:%s %s", dat_uniq$nodes, dat_uniq$cpu, dat_uniq$sub_type)
	detailed.labs.sub = sprintf("sub: %s", dat_uniq$sub_type)
	detailed.labs.dep = sprintf("dep: %s", dat_uniq$dep_type)
	## --------------- start plotting
	par(mar = c(0, 0, 0, 0)) ## how much margin to leave around...
	openplotmat()
	
	## -------------------------------- a r r o w s ------------------------------- ##
	if (nrow(dat_dep>0)){
		for (i in 1:nrow(dat_dep)){
			to = elpos[dat_dep$jobid[i], ];
			from = elpos[dat_dep$prev_jobid[i], ]
			if (dat_dep$dep_type[i]=="gather"){
				for(curve in curves)
					curvedarrow (to = to, from = from, lwd = arr.lwd, arr.pos = arr.pos,
											 arr.length = arr.len,
											 segment=c(0.2,0.8), curve=curve, arr.col=arr.col, lcol=arr.col)
			}
			straightarrow (to = to, from = from, lwd = arr.lwd, arr.pos = arr.pos, arr.length = arr.len,
										 arr.col=arr.col, lcol=arr.col)
		}
	}
	
	## -------------------------------- b o x e s ------------------------------- ##
	for (i in 1:nrow(dat_uniq)){
		lab=dat_uniq$jobname[i]
		if (dat_uniq$sub_type[i]=="scatter"){shadow.sizes=shadow.sizes.scatter
		}else{shadow.sizes=shadow.sizes.serial}
		for(shadow in shadow.sizes)
			textrect(elpos[i,], radx=boxwd, rady=boxht, lab = lab, shadow.col = shadow.col,
							 shadow.size = shadow, lcol=box.lcol,cex = cex, col=textcol)
		if (detailed){
			textplain(elpos[i,] + detail.offset, boxht, lab = detailed.labs.dep[i],
								cex=cex_detail, col=textcol)
			textplain(elpos[i,] - detail.offset, boxht, lab = detailed.labs.sub[i],
								cex=cex_detail, col=textcol)
		}
	}
	if (pdf) dev.off()
}

#' @importFrom grDevices dev.off
#' @importFrom stats complete.cases
.plot_flow_dat_type2 <- function(x,
																 detailed = FALSE,
																 pdf = FALSE,
																 pdffile=sprintf("flow.pdf"),
																 width, height,
																 curve = 0.5,
																 arr.type = "simple",
																 arr.lcol = "gray26",
																 arr.col = "gray26", ## arraow
																 segment.from = 0.1,
																 segment.to = 0.9,
																 cex.txt = 0.8, ## labels
																 arr.pos = 0.9,
																 box.prop = 0.15,
																 box.cex = 0.7,
																 box.type = "rect",
																 box.lwd = 0.6,
																 shadow.size = 0,
																 box.lcol = "lightskyblue4",
																 relsize = 0.85,
																 ...){
	
	if (missing(height))  height = 2.5 * nrow(x)
	if (missing(width)) width = 2 * nrow(x)
	if (nrow(x) < 2) return(c("need a few more jobs.."))
	
	x = arrange_flowdef(x)
	
	jobnames=unique(as.c(x$jobname))
	dat_dep <- x[complete.cases(x),]
	#dat_uniq <- x[sapply(jobnames, function(j) which(x$jobname==j)[1]),]
	m <- matrix(0, nrow = length(jobnames), ncol = length(jobnames))
	colnames(m) = rownames(m) = jobnames
	## -------- get positions
	#disp_mat <- table(ifelse(is.na(dat_uniq$prev_jobid), 0, dat_uniq$prev_jobid))
	dat_dep$dep_type = ifelse(dat_dep$dep_type %in% c(".", "none") |
															is.na(dat_dep$dep_type) | is.null(dat_dep$dep_type), 0, dat_dep$dep_type)
	for(i in 1:nrow(dat_dep)){
		m[dat_dep$jobname[i], dat_dep$prev_jobs[i]] = dat_dep$dep_type[i]
	}
	##### some options
	if (pdf){
		#box.prop = 0.15, box.cex = 0.7, box.type = "rect", box.lwd = 0.6, shadow.size = 0, box.lcol = "lightskyblue4",
	}
	if (pdf) pdf(file=pdffile, width = width, height = height)
	plotmat(m,
					curve = curve, arr.type = arr.type, arr.lcol = arr.lcol, arr.col = arr.col, ## arraow
					segment.from = segment.from, segment.to = segment.to, arr.pos = arr.pos,
					cex.txt = cex.txt, ## labels
					box.prop = box.prop, box.cex = box.cex, box.type = box.type, box.lwd = box.lwd,
					shadow.size = shadow.size, box.lcol = box.lcol, relsize = relsize, ...) ## box
	if (pdf) dev.off()
}

#' @rdname plot_flow
#' @export
plot.flowdef = plot_flow.flowdef

#' @rdname plot_flow
#' @export
plot.flow = plot_flow.flow
