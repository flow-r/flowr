setGeneric("to_flow", function (x, ...){
	standardGeneric("to_flow")
})

setClass("flow_def", contains = "data.frame") 
#http://www.carlboettiger.info/2013/09/11/extending-data-frame-class.html

#' @export
check <- function(x, ...) {
	UseMethod("check")
}

is.flow_def <- function(x){
	class(x) == "flow_def"
}

#' @export
#' @importFrom knitr kable
check.flow_def <- function(x, 
	sub_types = c("serial", "scatter", "burst"),
	dep_types = c("none", "serial", "gather")){
		if(sum(!x$dep_type %in% dep_types)) 
			stop("Dependency type not recognized ", paste(x$dep_type, collapse = " "), 
				"should be from ", paste(dep_types, collapse = " "))
		if(sum(!x$sub_type %in% sub_types)) 
			stop("Submission type not recognized ", paste(x$sub_type, collapse = " "), 
				"should be from ", paste(sub_types, collapse = " "))
		## check if some jobs are put as dependencies but not properly defined
		x$prev_jobs = gsub("\\.|none", NA, x$prev_jobs)
		prev_jobs = unlist(strsplit(x$prev_jobs[!is.na(x$prev_jobs)], ","))
		miss_jobs = prev_jobs[!prev_jobs %in% x$jobname]
		if(length(miss_jobs) > 0) 
			stop("Some jobs do not exist: ", miss_jobs)
		## check if dep is none, but prev jobs defined
		rows = x$dep_type == "none" & !is.na(x$prev_jobs)
		if(sum(rows)){
			print(kable(x[rows,]))
			stop("\nPrevious jobs defined, but dependency type is none")
		}
		rows = x$dep_type != "none" & is.na(x$prev_jobs)
		if(sum(rows)){
			print(kable(x[rows,]))
			stop("Previous jobs NOT defined, but dependency type is NOT none")
		}
		#print(x)
		## check all previous jobs defined in names
		## code previous jobs as NA
		## allowable types:
		## previous job
		##      scatter --(serial)--> scatter
		##      scatter --(serial)--> scatter
		##      scatter --(gather)--> scatter
		##      scatter --(gather)--> serial
		##      serial  --(serial)--> scatter
		##      serial  --(burst)--> scatter
		## not allowed:
		##      any --(none)--> any
		invisible(x)
	}

#

as.flow_def <- function(x){
	if(is.flow_def(x))
		return(x)
	## ---- assuming x is a file
	if(is.data.frame(x))
		y <- new("flow_def", x)
	if(is.character(x))
		y <- new("flow_def", read_sheet(x, id_column = "jobname"))
	y = check(y)
}


#' @export
to_flow <- function(x, ...) {
	UseMethod("to_flow")
}

#' @export
to_flow.list <- function(x, def = 'flow_def'){
	def <- as.flow_def(def)
}

# def = system.file(package = "flowr", "files/flow_def_ex1.txt")

#' this operates on a single sample basis
#' @details subset the data.frame by sample and then supply to this function, if you want seperate flow for each sample
#' @export
to_flow.data.frame <- function(x, def, qobj, 
	platform,
	cpu = 1, walltime = "1:00", memory = "1g",
	flowname, desc, flow_base_path
	){
		## need a few columns
		
		if(is.null(x$jobname))
			stop("x does not have a column jobname")
		if(is.null(x$cmd))
			stop("x does not have a column cmd")
		
		x = data.frame(x, stringsAsFactors = FALSE)
		def = as.flow_def(def)
		## A check x should be in def 
		if(mean(!unique(x$jobname) %in% na.omit(def$jobname))){
			stop("Some jobs in x are not in flow_definition\n")
		}
		## B AND vice-versa
		if(mean(!na.omit(def$jobname) %in% unique(x$jobname))){
			stop("Some jobs in flow_def are not in x\n")
		}
		
		if(missing(qobj))
			qobj <- queue(type = platform, verbose = FALSE)
		cmd.list = split.data.frame(x, x$jobname)
		
		jobs <- lapply(1:length(cmd.list), function(i){
			#jobs = lapply(1:length(cmd.list), function(i){
			message(".", appendLF = FALSE)
			cmds = cmd.list[[i]]$cmd; jobnm = names(cmd.list)[i]
			#cmds = unique(cmds);
			def2 = subset(def, def$jobname == jobnm)
			prev_job = unlist(def2$prev_jobs)
			if(!is.na(prev_job))
				prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
			d_cpu = unlist(def2$cpu_reserved)
			d_walltime = unlist(def2$walltime)
			d_memory = as.character(unlist(def2$memory_reserved))
			d_queue = unlist(def2$queue)
			d_dep_type = unlist(def2$dep_type)
			d_sub_type = unlist(def2$sub_type)
			if(length(d_sub_type) == 0)
				sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
			## guess dep_type
			if(length(prev_job) > 1){
				dep_type = "gather"
			}else if(length(cmd.list[[prev_job]]) == 0){
				dep_type = "none"
			}else if(length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
				dep_type = "serial"
			}else if(length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
				dep_type = "gather"
			}else if(length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
				dep_type = "burst"
			}
			##dep_type = unlist(subset(infomat, jobname == jobnm, select = 'sub_type'))
			##sub_type = "serial"
			## -------- if cmds are missing; change to echo 0 and make cpu = 1
			cpu = ifelse(cmds[1] == ".", 1, cpu)
			cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1]) ## if starts from . echo
			j = job(q_obj = qobj, name = jobnm, previous_job = prev_job, cmds = cmds,
				dependency_type = d_dep_type, submission_type = d_sub_type,
				cpu = d_cpu, queue = d_queue,
				walltime = d_walltime, memory = d_memory)
			return(j)
		})
		fobj <- flow(jobs = jobs,
			desc = desc, name = flowname,
			mode = "scheduler", 
			flow_base_path = flow_base_path)
		return(fobj)
	}

#setMethod("to_flow", signature(x = "list"), definition = .to_flow.list)
#setMethod("to_flow", signature(x = "data.frame"), definition = .to_flow.data.frame)


#' @title to_flow
#' @description Create a \link{flow} object from a list of commands
#' @param x \link{list} of commands
#' @param def A flow definition table \link{flow_def}. Basically a table with resource requirements and mapping of the jobs in this flow
#' @param q a object of class \link{queue}, defining how to submit the jobs. This can be created from parameters specified in system wide config file (~/flowr/
#' @param samplename name of the sample
#' @param flowname name of the flow
#' @param execute whether to submit the flow to the cluster after creation
#' @param flow_base_path base path for log file etc. Basically the main operating folder for this flow.
#' @export
cmds_to_flow <- function(cmd.list,
	samplename = "",
	infomat,
	q_obj = queue(type = "lsf", verbose=FALSE),
	flowname = "stage2",
	execute = FALSE,
	flow_base_path = "/scratch/iacs/flow_pipe/tmp"){
		## trim down the list
		cmd.list = lapply(cmd.list, function(y) Filter(function(x) !x == "", y))
		infomat$dep_type = ifelse(infomat$previous_job==".", "none", "serial")
		#infomat$previous_jobs = ifelse(infomat$previous_job==".", NULL, infomat$previous_job) ## prev job null
		#infomat <- cbind(jobnames, sub_type, cpus, prev_jobs, dep_type)
		## Error handling: missing in info
		missing.info = names(cmd.list)[!names(cmd.list) %in% infomat$jobname]
		if(length(missing.info) > 0){
			warning("\n\nMessage:\nOops issue with infomat.\n",
				"It seems these job names are missing from the infomat:\n",
				paste(missing.info, collapse="\n"),
				".\nWill remove them from cmd.list and proceed\n")
			## removing from commands if missing in infomat
			cmd.list = cmd.list[names(cmd.list) %in% infomat$jobname]
		}
		## chck if prev jobs have a job defined
		prev_jobs = strsplit(infomat$previous_job, ",")
		missing.prev = infomat$previous_job[!unlist(prev_jobs) %in% c(infomat$jobname, ".", NA, "NA")]
		if(length(missing.prev) > 0)
			stop("\n\nMessage:\nOops issue with infomat.\n",
				"It seems these previous job names are missing from the jobname column:\n",
				"Dependencies are only supported within the same flow.\n",
				missing.prev, "\n")
		if( mean(table(infomat$jobname)) != 1 || mean(table(names(cmd.list))) != 1 )
			stop("\n\nMessage:\nOops issue with infomat/cmd.list.\n",
				"Seem either jobnames in infomat are or that in cmd.list are duplicated\n")
		
		jobs = list()
		for( i in 1:length(cmd.list)){
			#jobs = lapply(1:length(cmd.list), function(i){
			#message(".")
			cmds = cmd.list[[i]]; jobnm = names(cmd.list)[i]
			#cmds = unique(cmds);
			infomat2 = subset(infomat, infomat$jobname == jobnm)
			#print(knitr:::kable(infomat2))
			prev_job = unlist(infomat2$previous_job)
			prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
			cpu = unlist(infomat2$cpu_reserved)
			walltime = unlist(infomat2$walltime)
			memory = as.character(unlist(infomat2$memory_reserved))
			queue = unlist(infomat2$queue)
			dep_type = unlist(infomat2$dep_type)
			sub_type = unlist(infomat2$sub_type)
			if(length(sub_type) == 0)
				sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
			## guess dep_type
			if(length(prev_job) > 1){
				dep_type = "gather"
			}else if(length(cmd.list[[prev_job]]) == 0){
				dep_type = "none"
			}else if(length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
				dep_type = "serial"
			}else if(length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
				dep_type = "gather"
			}else if(length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
				dep_type = "burst"
			}
			##dep_type = unlist(subset(infomat, jobname == jobnm, select = 'sub_type'))
			##sub_type = "serial"
			## -------- if cmds are missing; change to echo 0 and make cpu = 1
			cpu = ifelse(cmds[1] == ".", 1, cpu)
			cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1]) ## if starts from . echo
			j = job(q_obj = q_obj, name = jobnm, previous_job = prev_job, cmds = cmds,
				dependency_type = dep_type, submission_type = sub_type,
				cpu = cpu, queue = queue,
				walltime = walltime, memory = memory)
			jobs = c(jobs, j)
		}
		f_obj <- flow(jobs = jobs,
			desc=sprintf("%s-%s", flowname, samplename), name = flowname,
			mode="scheduler", flow_base_path = flow_base_path)
		len = length(jobs)
		#debug(flow:::.submit_flow)
		#mypack:::reload('flow')
		f_obj_uuid <- .submit_flow(f_obj, execute = execute, make_flow_plot = TRUE)
		#   if(sum(flow:::.create_jobs_mat(f_obj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
		#     cat("Plotting...\n")
		#     try(flow:::.plot_flow(x = f_obj, type = '1',
		#                           pdf = TRUE, pdffile = file.path(f_obj_uuid@flow_path, "flow_design.pdf")))
		#   }
		return(f_obj_uuid)
	}


