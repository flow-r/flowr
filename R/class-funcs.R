
#### -----------------------

setGeneric("test_queue", function(q_obj, ...){
    standardGeneric("test_queue")
})

setGeneric("create_queue_cmd", function (q_obj, file, ...){
    standardGeneric("create_queue_cmd")
})

setGeneric("submit_job", function (j_obj, f_obj, ...){
    standardGeneric("submit_job")
})

## setGeneric("submit_job", function (j_obj, f_obj, ...){
##     standardGeneric("submit_job")
## })

setMethod("test_queue", signature(q_obj= "queue"), function (q_obj, verbose = FALSE){
    cmd.0 <- create_queue_cmd(q_obj)
    if(verbose) print(cmd.0)
    cmd <- sprintf("echo 'sleep 1' | %s", cmd.0)
    if( verbose ) print (cmd)
    system(cmd)
})


setMethod("create_queue_cmd", signature(q_obj = "queue"), function (q_obj, ...){
    q_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",q_obj@dependency, collapse="", sep=""))
    l <- slots_as_list(q_obj, names=slotNames("queue"))
    l <- l[! names(l) %in% c("format","type")] ### ignore a few of the slots
    names(l) = toupper(names(l)) ## get list of slots
    l <- c("CMD"=file)
    .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
    cmd <- system(sprintf("eval echo %s ",q_obj@format),intern=TRUE)
    return(cmd=cmd)
})
#cmd <- sprintf("%s %s",create_queue_cmd(j_obj), file=files[i])


setMethod("create_queue_cmd", signature(q_obj = "queue", file="character"), function (q_obj, file, index, ...){
    #q_obj=j_obj
    if(q_obj@dependency_type=="gather"){
        q_obj@dependency <- sprintf("-W depend=afterok:%s",paste(q_obj@dependency, collapse=":"))
    }else if (q_obj@dependency_type=="serial"){
        q_obj@dependency <- sprintf("-W %s",paste(" depend=afterok:",q_obj@dependency[index], sep=""))
    }else{
        q_obj@dependency <- ""
    }
    l <- slots_as_list(q_obj, names=slotNames("queue"))
    l <- l[! names(l) %in% c("format","type")] ### ignore a few of the slots
    names(l) = toupper(names(l)) ## get list of slots
    l <- c(l, "CMD"=file)
    .Internal(Sys.setenv(names(l), as.character(unlist(l)))) ## set slots in BASH
    cmd <- system(sprintf("eval echo %s ",q_obj@format),intern=TRUE)
    return(cmd=cmd)
})
## trace("create_queue_cmd", browser, exit=browser, signature = c("queue","character"));
## cmd <- create_queue_cmd(j_obj, file=files[i])
## untrace("create_queue_cmd", signature = c("queue","character"));


#### ----------------------- submit job as part of a flow, this would be called from function flow
setMethod("submit_job", signature(j_obj = "job", f_obj = "flow"), function (j_obj, f_obj,
                                                     execute = FALSE, verbose = TRUE, wd, ...){
    wd <- file.path(f_obj@flow_base_path, j_obj@name) ## j_obj@name: is indexed
    dir.create (wd, recursive=TRUE, showWarnings=FALSE);dir.create(file.path(f_obj@flow_base_path,"trigger"), showWarnings=FALSE)
    trigger_path <- f_obj@trigger_base_path ## comes from the flow
    j_obj@stderr <- wd;j_obj@stdout <- wd;j_obj@cwd <- dirname(wd)## FLOWBASE
    ##### this would be based on number of submission type
    if(j_obj@submission_type %in% c("scatter","serial")){
        files <- sprintf("%s/%s_cmd_%s.sh",wd, j_obj@name, 1:length(j_obj@cmds))
        jobids <- sapply(1:length(j_obj@cmds), function(i){
            beforescript <- c("echo 'BGN at' `date`")
            afterscript <- c(sprintf("echo $? > %s/trigger/trigger_%s_%s.txt", f_obj@flow_base_path, j_obj@name,i),
                             "echo 'END at' `date`")
            script <- c(j_obj@cmds[i], afterscript)
            write(script, files[i])
            ## make a long job name to capture the run
            obj <- j_obj;obj@name <- sprintf("%s-%s_%s",basename(f_obj@flow_base_path),j_obj@name,i)
            cmd <- create_queue_cmd(obj, file=files[i], index=i)
            if(execute){
                jobid <- system(cmd, intern = TRUE)
                return(jobid)
            } ## execute
            return(cmd) ## if no execute return the cmd
        }) ## for loop
        j_obj@id <- jobids
    }## submissiontype
    return(j_obj)
})


submit_flow <- function(f_obj){
    f_obj@flow_base_path <- sprintf("%s/aln_merge-%s",f_obj@flow_base_path,UUIDgenerate())
    jobnames <- sapply(f_obj@jobs, function(x) x@name)
    names(f_obj@jobs) <- jobnames
    dir.create(file.path(f_obj@flow_base_path,"tmp"), showWarnings=FALSE, recursive=TRUE)
    for(i in 1:length(f_obj@jobs)){
        ## ------ create the name of the job with its index in the supplied flow
        f_obj@jobs[[i]]@name <- sprintf("%s.%s",i,f_obj@jobs[[i]]@name)
        ## ------ check if there are any dependencies
        previous_job <- f_obj@jobs[[i]]@previous_job
        f_obj@jobs[[previous_job]]@dependency <- f_obj@jobs[[i]]@id
        ## ------ submit the job
        f_obj@jobs[[i]] <- submit_job(f_obj@jobs[[i]], f_obj, execute=TRUE)
        ## ------ check if this is the last job in the flow
        if(i < length(f_obj@jobs)){
            next_job <- f_obj@jobs[[i]]@next_job
            f_obj@jobs[[nextjob]]@dependency <- f_obj@jobs[[i]]@id
        }
    }
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

##submit.job

create_batch_command <- function(){

}
