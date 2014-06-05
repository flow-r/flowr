
#### -----------------------

setGeneric("test_queue", function(q_obj, ...){
    standardGeneric("test_queue")
})

setGeneric("create_queue_cmd", function (q_obj, file, ...){
    standardGeneric("create_queue_cmd")
})

setGeneric("create_queue_cmd", function (j_obj, file, ...){
    standardGeneric("create_queue_cmd")
})

setGeneric("submit_job", function (j_obj, f_obj, ...){
    standardGeneric("submit_job")
})

setGeneric("submit_flow", function (f_obj, ...){
    standardGeneric("submit_flow")
})

if (!isGeneric("plot"))
    setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("test_queue", signature(q_obj= "queue"), function (q_obj, verbose = FALSE){
    cmd.0 <- create_queue_cmd(q_obj)
    if(verbose) print(cmd.0)
    cmd <- sprintf("echo 'sleep 1' | %s", cmd.0)
    if( verbose ) print (cmd)
    system(cmd)
})


## setMethod("create_queue_cmd", signature(q_obj = "queue"), function (q_obj, ...){
##     if(q_obj@dependency_type=="gather"){
##         if(q_obj@type=="torque")
##             q_obj@dependency <- sprintf("-W depend=afterok:%s",paste(q_obj@dependency, collapse=":"))
##         else if(q_obj@type=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",paste(q_obj@dependency, sep=" && "))
##     }else if (q_obj@dependency_type=="serial"){
##         if(q_obj@type=="torque")
##             q_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",q_obj@dependency[index], sep=""))
##         else if(q_obj@type=="lsf")
##             q_obj@dependency <- sprintf("-w '%s'",q_obj@dependency[index])
##     }else{
##         q_obj@dependency <- ""
##     }
##     l <- slots_as_list(q_obj, names=slotNames("queue"))
##     l <- l[! names(l) %in% c("format","type")] ### ignore a few of the slots
##     names(l) = toupper(names(l)) ## get list of slots
##     ## l <- c("CMD"=cmd)
##     .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
##     cmd <- system(sprintf("eval echo %s ",q_obj@format),intern=TRUE)
##     return(cmd=cmd)
## })
## #cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file=files[i])

.create_queue_cmd <- function(j_obj, file, index, ...){
   ## ----- this job depends on multiple jobs. create a string with multiple job ids
    if(j_obj@dependency_type=="gather"){
        if(j_obj@type=="torque")
            j_obj@dependency <- sprintf("-W depend=afterok:%s",paste(j_obj@dependency, collapse=":"))
        else if(j_obj@type=="lsf")
            j_obj@dependency <- sprintf("-w '%s'",paste(j_obj@dependency, collapse=" && "))
    }else if (j_obj@dependency_type=="serial"){
        if(j_obj@type=="torque")
            j_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",j_obj@dependency[index], sep=""))
        else if(j_obj@type=="lsf")
            j_obj@dependency <- sprintf("-w '%s'",j_obj@dependency[index])
    }else{
        j_obj@dependency <- ""
    }
    l <- slots_as_list(j_obj, names=slotNames("queue"))
    l <- l[! names(l) %in% c("format","type")] ### ignore a few of the slots
    names(l) = toupper(names(l)) ## get list of slots
    l <- c(l, "CMD"=file)
    .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
                                        #cmd <- system(sprintf("eval echo %s ",j_obj@format),intern=TRUE)
    cmd <- system(sprintf("echo %s ",j_obj@format),intern=TRUE)
    return(cmd=cmd)
}
setMethod("create_queue_cmd", signature(j_obj = "job", file="character"), definition=.create_queue_cmd)


#### --------------------- submit job as part of a flow, this would be called from function flow
.submit_job <- function (j_obj, f_obj,execute = FALSE, verbose = TRUE, wd, job_id,...){
    ## ========= create the name of the job with its index in the supplied flow
    j_obj@jobname <- sprintf("%s.%s",job_id,j_obj@name)
    wd <- file.path(f_obj@flow_base_path, j_obj@jobname) ## j_obj@name: is indexed
    dir.create (wd, recursive=TRUE, showWarnings=FALSE);
    dir.create(file.path(f_obj@flow_base_path,"trigger"), showWarnings=FALSE)
    trigger_path <- f_obj@trigger_base_path ## comes from the flow
    j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- file.path(dirname(wd),"tmp") ## FLOWBASE
    ## ------------ this would be based on number of submission type
    if(j_obj@submission_type %in% c("serial")){
        j_obj@cmds <-  paste("## ------", names(j_obj@cmds), "\n", j_obj@cmds, "\n\n", collapse="")
    }
    ## if(j_obj@submission_type %in% c("scatter")){
    files <- sprintf("%s/%s_cmd_%s.sh",wd, j_obj@name, 1:length(j_obj@cmds))
    jobids <- sapply(1:length(j_obj@cmds), function(i){
        beforescript <- c("#!/bin/env bash","echo 'BGN at' `date`")
        afterscript <- c(sprintf("echo $? > %s/trigger/trigger_%s_%s.txt", f_obj@flow_base_path, j_obj@name,i),
                         "echo 'END at' `date`")
        script <- c(beforescript,j_obj@cmds[i], afterscript)
        write(script, files[i])
        ## -------   make a long job name to capture the run
        obj <- j_obj;
        obj@jobname <- sprintf("%s-%s_%s",basename(f_obj@flow_base_path),j_obj@jobname,i)
        cmd <- create_queue_cmd(obj, file=files[i], index=i)
        if(execute){
            jobid <- system(cmd, intern = TRUE)
            return(jobid)
        } ## execute
        return(cmd) ## if no execute return the cmd
    }) ## for loop
    if(j_obj@type=="lsf" & execute)
        j_obj@id <- gsub(".*(\\<[0-9]*\\>).*","\\1",jobids)
    else
        j_obj@id <- jobids
    ## }## submissiontype
    return(j_obj)
}
setMethod("submit_job", signature(j_obj = "job", f_obj = "flow"),definition=.submit_job)


.submit_flow <- function(f_obj, attach_uuid=TRUE, execute=FALSE){
    if(attach_uuid) f_obj@flow_base_path <- sprintf("%s/%s-%s",f_obj@flow_base_path, f_obj@name, UUIDgenerate())
    jobnames <- sapply(f_obj@jobs, function(x) x@name)
    names(f_obj@jobs) <- jobnames
    dir.create(file.path(f_obj@flow_base_path,"tmp"), showWarnings=FALSE, recursive=TRUE)
    for(i in 1:length(f_obj@jobs)){
        ## ------ check if there are any dependencies
        previous_job <- f_obj@jobs[[i]]@previous_job
        if(length(previous_job)!=0)
            ## f_obj@jobs[[i]]@dependency <- f_obj@jobs[[previous_job]]@id
            ## -------- can have multiple dependencies
            f_obj@jobs[[i]]@dependency <- unlist(sapply(previous_job, function(x) f_obj@jobs[[x]]@id))
        ## ------ submit the job
        f_obj@jobs[[i]] <- submit_job(f_obj@jobs[[i]], f_obj, execute=execute, job_id=i)
        ## ------ check if this is NOT last job in the flow
        ## if(i < length(f_obj@jobs)){
        ##     next_job <- f_obj@jobs[[i]]@next_job
        ##     if(length(next_job)!=0)     #if we have the next job
        ##         f_obj@jobs[[next_job]]@dependency <- f_obj@jobs[[i]]@id
        ## }
    }
    return(f_obj)
}
setMethod("submit_flow", signature(f_obj = "flow"),definition=.submit_flow)

## ------------- make a flowchart using the object
.plot_flow <- function(x, detailed=FALSE, ...){
    require(diagram)
    jobnames <- sapply(x@jobs, slot, "name")
    prev_jobs <- sapply(x@jobs, slot, "previous_job")
    prev_jobs <- sapply(prev_jobs, function(x) ifelse(length(x) > 0, paste(x,collapse=","), NA))
    dep_type <- sapply(x@jobs, slot, "dependency_type")
    sub_type <- sapply(x@jobs, slot, "submission_type")
    cpu <- sapply(x@jobs, slot, "cpu")
    nodes <- sapply(x@jobs, slot, "nodes")
    dat <- cbind(jobnames, prev_jobs, dep_type, sub_type, cpu, nodes)
    dat <- as.data.frame(dat)
    ## ----------- handle cases where we have multiple dependencies
    rows <- grep(",",dat$prev_jobs)
    if(length(rows)>0){
      dat2 <- data.frame()
      for(row in rows){
        prev_jobs=strsplit(as.c(dat[row,]$prev_jobs),",")[[1]]
        dat2 <- rbind(dat2,cbind(jobnames=dat[row,1],prev_jobs=prev_jobs,dat[row,3:4]))
      }
      dat <- rbind(dat[-rows,],dat2)
    }
    ## ----------- get ids and put them in the db
    jobnames=unique(as.c(dat$jobnames))
    jobid <- 1:length(jobnames);names(jobid)=jobnames
    prev_jobid <- jobid[as.c(dat$prev_jobs)]
    dat$jobid <- jobid[as.c(dat$jobnames)];dat$prev_jobid <- prev_jobid

    ## ## ---------- order them so that they are plotted well; REPEAT
    ## dat <- dat[order(dat$prev_jobid, dat$jobid, na.last=FALSE, decreasing=FALSE),]
    ## jobid <- 1:length(jobnames);names(jobid)=jobnames
    ## prev_jobid <- jobid[as.c(dat$prev_jobs)]
    ## dat$jobid <- jobid[dat$jobnames];dat$prev_jobid <- prev_jobid

    ## -------- get positions
    elpos <- coordinates (c(1, table(dat$prev_jobid)))
    dat_compl <- dat[complete.cases(dat),]
    ## fromto <- cbind(dat$prev_jobid, dat$jobid)
    ## fromto <- fromto[complete.cases(fromto),]
    par(mar = c(1, 1, 1, 1))
    openplotmat()
    ##arrpos <- matrix(ncol = 2, nrow = nrow(fromto))
    for (i in 1:nrow(dat_compl)){
        linewd=2;arrow_length=0.6;
        to=elpos[dat_compl$jobid[i], ]; from=elpos[dat_compl$prev_jobid[i], ]
        if(dat_compl$dep_type[i]=="gather"){
            linewd=2;arrow_length=0.8
            curvedarrow (to = to, from = from, lwd = 2, arr.pos = 0.6, arr.length = 0.6, segment=c(0.2,0.8), curve=0.2)
            curvedarrow (to = to, from = from, lwd = 2, arr.pos = 0.6, arr.length = 0.6, segment=c(0.2,0.8), curve=-0.2)
        }
        straightarrow (to = to, from = from, lwd = linewd, arr.pos = 0.6, arr.length = arrow_length)
    }
    for (j in dat$jobnames){
        i=which(dat$jobnames==j)[1]
        shadow.col <- "lightskyblue4";boxsize=0.06;textsize=1.1
        if(dat$sub_type[i]=="scatter"){## make it look like a array
            textrect(elpos[i,], boxsize, lab = dat$jobnames[i],shadow.col = shadow.col, shadow.size = 0.001, cex = textsize)
            textrect(elpos[i,], boxsize, lab = dat$jobnames[i],shadow.col = shadow.col, shadow.size = 0.005, cex = textsize)
            textrect(elpos[i,], boxsize, lab = dat$jobnames[i],shadow.col = shadow.col, shadow.size = 0.009, cex = textsize)
            textrect(elpos[i,], boxsize, lab = dat$jobnames[i],shadow.col = shadow.col, shadow.size = 0.013, cex = textsize)
        }else{
            textrect(elpos[i,], boxsize, lab = dat$jobnames[i],shadow.col = shadow.col, shadow.size = 0.005, cex = textsize)
        }
        lab <- sprintf("%s:%s %s",dat$nodes[i], dat$cpu[i], dat$sub_type[i])
        if(detailed) textplain(elpos[i,] - c(0,0.04), boxsize, lab = lab,cex=0.8)
    }
}
setMethod("plot", signature(x = "flow"), definition=.plot_flow)


create_batch_command <- function(){

}

#### ----------------------- submit loner job
setMethod("submit_job", signature(j_obj = "job"),
function (j_obj, execute = FALSE,verbose = TRUE, wd, ...){
    require(uuid)
    ## if(verbose) cat(j_obj@base_path, j_obj@name, "\n")
    if(missing(wd)){
        wd <- file.path(j_obj@base_path,paste(j_obj@name,
                                              uuid::UUIDgenerate(),sep="_"))
    }
    dir.create(wd, recursive=TRUE)
    script <- c(j_obj@cmd, sprintf("echo $? > %s/trigger_%s.txt", wd,j_obj@name))
    file <- sprintf("%s/%s.sh", wd, j_obj@name)
    write(script, file)
    j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- wd
    cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file)
    if (verbose) print(cmd)
    if(execute){
        jobid <- system(cmd, intern = TRUE)
        j_obj@id <- jobid
    }
    return(j_obj)
})
## trace("create_queue_cmd", browser, exit=browser, signature = c("queue","character"));
## cmd <- create_queue_cmd(j_obj, file=files[i])
## untrace("create_queue_cmd", signature = c("queue","character"));
