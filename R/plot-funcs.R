.create_jobs_mat <- function(x){
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

## ------------- make a flowchart using the object
.plot_flow <- function(x, detailed=FALSE, pdf=FALSE, pdffile=sprintf("%s.pdf",x@name), ...){
  require(diagram)
  dat <- .create_jobs_mat(x)
  .plot_flow_dat(x=dat, detailed=FALSE, pdf=FALSE, pdffile=sprintf("%s.pdf",x@name), ...)
}
setMethod("plot", signature(x = "flow"), definition=.plot_flow)


.plot_flow_dat <- function(x, detailed=FALSE, pdf=FALSE, pdffile=sprintf("flow.pdf"), ...){
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
  detailed.labs = sprintf("%s:%s %s", dat_uniq$nodes, dat_uniq$cpu, dat_uniq$sub_type)
  detail.cex=0.8; detail.offset=c(0,0.04)
  ## --------------- start plotting
  par(mar = c(1, 1, 1, 1))
  if(pdf) pdf(file=pdffile)
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
      textplain(elpos[i,] - detail.offset, boxsize, lab = detailed.labs[i], 
                cex=detail.cex, col=textcol)
    }
  }
  if(pdf) dev.off()
}
