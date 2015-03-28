#' @title create_jobs_mat
#' @description create_jobs_mat
#' @param x a \link{flow} object.
#' @keywords internal
#' @examples \dontrun{
#' .create_jobs_mat(x = x)}
create_jobs_mat <- function(x){
  jobnames <- sapply(x@jobs, slot, "name")
  prev_jobs <- sapply(x@jobs, slot, "previous_job")
  prev_jobs <- sapply(prev_jobs, function(x) ifelse(length(x) > 0, paste(x,collapse=","), NA))
  dep_type <- sapply(x@jobs, slot, "dependency_type")
  sub_type <- sapply(x@jobs, slot, "submission_type")
  cpu <- sapply(x@jobs, slot, "cpu")
  nodes <- sapply(x@jobs, slot, "nodes")
  dat <- cbind(jobnames, prev_jobs, dep_type, sub_type, cpu, nodes)
  dat <- as.data.frame(dat, stringsAsFactors=FALSE)
  ## ----------- handle cases where we have multiple dependencies
  rows <- grep(",",dat$prev_jobs)
  if(length(rows)>0){
    dat2 <- data.frame()
    for(row in rows){
      prev_jobs=strsplit(as.c(dat[row,]$prev_jobs),",")[[1]]
      dat2 <- rbind(dat2,cbind(jobnames=dat[row,"jobnames"], prev_jobs=prev_jobs,
                               dep_type=dat[row,"dep_type"],sub_type=dat[row,"sub_type"],
                               cpu=dat[row,"cpu"],nodes=dat[row,"nodes"]))
    }
    dat <- rbind(dat[-rows,],dat2)
  }
  for(j in 1:4){
    jobnames=unique(as.c(dat$jobnames))
    jobid <- 1:length(jobnames);names(jobid)=jobnames
    prev_jobid <- jobid[as.c(dat$prev_jobs)]
    dat$jobid <- jobid[as.c(dat$jobnames)];dat$prev_jobid <- prev_jobid
    dat <- dat[order(dat$prev_jobid, dat$jobid, na.last=FALSE, decreasing=FALSE),]
  }
  return(dat)
}
.create_jobs_mat=create_jobs_mat

## ------------- make a flowchart using the object
.plot_flow <- function(x, detailed = TRUE, pdf = FALSE, pdffile=sprintf("%s.pdf",x@name), type = c('1','2'), ...){
	type = match.arg(type)
  dat <- .create_jobs_mat(x)
  switch(type,
  			 '1' = .plot_flow_dat_type1(x=dat, detailed = detailed, pdf = pdf, pdffile=pdffile, ...),
  			 '2' = .plot_flow_dat_type2(x=dat, detailed = detailed, pdf = pdf, pdffile=pdffile, ...))
}

setGeneric("plot_flow", function (x, ...){
  standardGeneric("plot_flow")
})

#' @title plot_flow
#' @description plot the flow object
#' @aliases plot_flow plot_flow-method
#' @aliases plot
#' @param x Object of class \code{flow}
#' @param detailed include some details
#' @param pdf create a pdf instead of plotting interactively
#' @param pdffile output file name for the pdf file
#' @param type 1 is original, and 2 is a elipse with less details
#' @param ... experimental
#' @exportMethod plot_flow
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
#' ### Burst: one to many relationship
#' jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial", name = "job1")
#' jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
#'              dependency_type = "burst", previous_job = "job1")
#' fobj <- flow(jobs = list(jobj1, jobj2))
#' plot_flow(fobj)
setMethod("plot_flow", signature(x = "flow"), definition=.plot_flow)
setMethod("plot", signature(x = "flow"), definition=plot_flow)

.plot_flow_dat_type1 <- function(x, detailed = FALSE, pdf = FALSE, pdffile=sprintf("flow.pdf"),
                           width, height, ...){
    if(missing(height))  height = 2.5 * nrow(x)
    if(missing(width)) width = 2 * nrow(x)
    if(nrow(x) < 2) return(c("need a few more jobs.."))
    jobnames=unique(as.c(x$jobnames))
    dat_compl <- x[complete.cases(x),]
    dat_uniq <- x[sapply(jobnames, function(j) which(x$jobnames==j)[1]),]
    ## -------- get positions
    disp_mat <- table(ifelse(is.na(dat_uniq$prev_jobid), 0, dat_uniq$prev_jobid))
    elpos <- coordinates (disp_mat)
    ## -------- graphic params:
    shadow.col <- "lightskyblue4";boxwd=0.08;boxht=0.04;box.lcol = "gray26"
    if(detailed) boxht=0.06
    shadow.sizes.scatter=seq(from=0.001, by=0.003, length.out=4);shadow.sizes.serial=c(0.004)
    arr.col="gray26";arr.lwd=3;arr.len=0.6; arr.pos=0.55
    curves=c(-0.2,0.2);arr.lwd.curve=2;
    textsize=1.1;textcol="gray30"
    detail.cex=0.8; detail.offset=c(0,0.04)
    ## ---- change params for pdf
    if(pdf){
        shadow.col <- "lightskyblue4";boxwd=0.08;boxht=0.03;box.lcol = "gray26"
        if(detailed) boxht=0.0
        shadow.sizes.scatter=seq(from=0.001, by=0.003, length.out=4);shadow.sizes.serial=c(0.004)
        arr.col="gray26";arr.lwd=3;arr.len=0.6; arr.pos=0.55
        curves=c(-0.2,0.2);arr.lwd.curve=2;
        textsize=1.1;textcol="gray30"
        detail.cex=0.8; detail.offset=c(0,0.04)
    }
    #detailed.labs = sprintf("%s:%s %s", dat_uniq$nodes, dat_uniq$cpu, dat_uniq$sub_type)
    detailed.labs.sub = sprintf("sub: %s", dat_uniq$sub_type)
    detailed.labs.dep = sprintf("dep: %s", dat_uniq$dep_type)
    ## --------------- start plotting
    par(mar = c(1, 1, 1, 1))
    if(pdf) pdf(file=pdffile, width = width, height = height)
    openplotmat()
    if(nrow(dat_compl>0)){
        for (i in 1:nrow(dat_compl)){
            to=elpos[dat_compl$jobid[i], ]; from=elpos[dat_compl$prev_jobid[i], ]
            if(dat_compl$dep_type[i]=="gather"){
                for(curve in curves)
                    curvedarrow (to = to, from = from, lwd = arr.lwd.curve, arr.pos = arr.pos,
                                 arr.length = arr.len,
                                 segment=c(0.2,0.8), curve=curve, arr.col=arr.col, lcol=arr.col)
            }
            straightarrow (to = to, from = from, lwd = arr.lwd, arr.pos = arr.pos, arr.length = arr.len,
                           arr.col=arr.col, lcol=arr.col)
        }
    }
    for (i in 1:nrow(dat_uniq)){
        lab=dat_uniq$jobnames[i]
        if(dat_uniq$sub_type[i]=="scatter"){shadow.sizes=shadow.sizes.scatter
                                        }else{shadow.sizes=shadow.sizes.serial}
        for(shadow in shadow.sizes)
            textrect(elpos[i,], radx=boxwd, rady=boxht, lab = lab, shadow.col = shadow.col,
                     shadow.size = shadow, lcol=box.lcol,cex = textsize, col=textcol)
        if(detailed){
          textplain(elpos[i,] + detail.offset, boxht, lab = detailed.labs.dep[i],
                    cex=detail.cex, col=textcol)
          textplain(elpos[i,] - detail.offset, boxht, lab = detailed.labs.sub[i],
                      cex=detail.cex, col=textcol)
        }
    }
    if(pdf) dev.off()
}
.plot_flow_dat = .plot_flow_dat_type1

.plot_flow_dat_type2 <- function(x, detailed = FALSE, pdf = FALSE, pdffile=sprintf("flow.pdf"),
																 width, height, 
																 curve = 0.5, arr.type = "simple", arr.lcol = "gray26", arr.col = "gray26", ## arraow
																 segment.from = 0.1, segment.to = 0.9, arr.pos = 0.9,
																 cex.txt = 0.8, ## labels
																 box.prop = 0.15, box.cex = 0.7, box.type = "rect", box.lwd = 0.6, shadow.size = 0, 
                                 box.lcol = "lightskyblue4", relsize = 0.85,...){
	if(missing(height))  height = 2.5 * nrow(x)
	if(missing(width)) width = 2 * nrow(x)
	if(nrow(x) < 2) return(c("need a few more jobs.."))
	jobnames=unique(as.c(x$jobnames))
	dat_compl <- x[complete.cases(x),]
	dat_uniq <- x[sapply(jobnames, function(j) which(x$jobnames==j)[1]),]
	m <- matrix(0, nrow = length(jobnames), ncol = length(jobnames))
	colnames(m) = rownames(m) = jobnames
	## -------- get positions
	disp_mat <- table(ifelse(is.na(dat_uniq$prev_jobid), 0, dat_uniq$prev_jobid))
	dat_compl$dep_type = ifelse(dat_compl$dep_type %in% c(".", "none") |
                                is.na(dat_compl$dep_type) | is.null(dat_compl$dep_type), 0, dat_compl$dep_type)
  for(i in 1:nrow(dat_compl)){ 
    m[dat_compl$jobnames[i], dat_compl$prev_jobs[i]] = dat_compl$dep_type[i]
  }
  ##### some options
  if(pdf){
    #box.prop = 0.15, box.cex = 0.7, box.type = "rect", box.lwd = 0.6, shadow.size = 0, box.lcol = "lightskyblue4",
	}
	if(pdf) pdf(file=pdffile, width = width, height = height)
	p <- plotmat(m, 
							 curve = curve, arr.type = arr.type, arr.lcol = arr.lcol, arr.col = arr.col, ## arraow
							 segment.from = segment.from, segment.to = segment.to, arr.pos = arr.pos,
							 cex.txt = cex.txt, ## labels
							 box.prop = box.prop, box.cex = box.cex, box.type = box.type, box.lwd = box.lwd, 
               shadow.size = shadow.size, box.lcol = box.lcol, relsize = relsize, ...) ## box
	if(pdf) dev.off()	
}


##----- plotmat of diagram
if(FALSE){
	jobnames=unique(as.c(x$jobnames))
	dat_compl <- x[complete.cases(x),]
	dat_uniq <- x[sapply(jobnames, function(j) which(x$jobnames==j)[1]),]
	m <- matrix(0, nrow = length(jobnames), ncol = length(jobnames))
	colnames(m) = rownames(m) = jobnames
	## -------- get positions
	disp_mat <- table(ifelse(is.na(dat_uniq$prev_jobid), 0, dat_uniq$prev_jobid))
	m[dat_compl$jobnames, dat_compl$prev_jobs] = dat_compl$dep_type
	p <- plotmat(m, 
					curve = 0.5, arr.type = "simple", arr.lcol = "gray26", arr.col = "gray26", ## arraow
					segment.from = 0.1, segment.to = 0.9, arr.pos = 0.9,
					cex.txt = 0.8, ## labels
					box.prop = 0.15, box.cex = 0.7, box.type = "rect", box.lwd = 0.6, shadow.size = 0, box.lcol = "lightskyblue4",
					relsize = 0.85) ## box
	

	
}
