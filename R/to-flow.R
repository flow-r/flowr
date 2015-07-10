
guess_sub_type <- function(cmds){
	sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
	return(sub_type)
}

guess_dep_type <- function(cmds, prev_job){
	if(length(prev_job) > 1){
		dep_type = "gather"
	}else if(length(x[[prev_job]]) == 0){
		dep_type = "none"
	}else if(length(x[[prev_job]]) == length(cmds)){
		## if same length, serial
		dep_type = "serial"
	}else if(length(x[[prev_job]]) > length(cmds) & length(cmds) == 1  ){
		## --- if prevous job were more than current
		dep_type = "gather"
	}else if(length(x[[prev_job]]) == 1 & length(cmds) > 1){ 
		## previous was only one, and current are a few
		dep_type = "burst"
	}
	return(dep_type)
	
}

#' @title 
#' Create flow objects
#' @description 
#' Use a set of shell commands and flow definiton to create \link{flow} object.
#' @param x path (char. vector) to flow_mat, a data.frame or a list.
#' @param def A flow definition table. Basically a table with resource requirements and mapping of the jobs in this flow
#' @param platform character vector, specifying the platform to use. local, lsf, torque, moab, sge, slurm, ...
#' This over-rides the platform column in flow_def.
#' @param grp_col column name used to split x (flow_mat). Default: `samplename`
#' @param jobname_col column name with job names. Default: `jobname`
#' @param cmd_col column name with commands. Default: `cmd`
#' @param flowname name of the flow
#' @param flow_run_path Path to a folder. Main operating folder for this flow. Default it `getOption("flow_run_path")`.
#' 
#' @param submit Depreciated. Use submit_flow on flow object this function returns. TRUE/FALSE
#' @param execute Depreciated. Use submit_flow on flow object this function returns. TRUE/FALSE, an paramter to submit_flow()
#' @param qobj Depreciated. A object of class \link{queue}.
#' 
#' 
#' 
#' @details The parameter x can be a path to a flow_mat, or a data.frame (as read by read_sheet).
#' This is a minimum three column matrix with:
#' 
#' samplename<TAB>jobname<TAB>cmd
#' 
#' @export
to_flow <- function(x, def, 
	grp_col,
	jobname_col,
	cmd_col,
	flowname,
	flow_run_path,
	submit = FALSE,
	platform,
	execute,
	qobj, ...) {
	message("input x is ", class(x))
	UseMethod("to_flow")
	warnings()
}

#' @inheritParams to_flow.data.frame
#' @export
to_flow.vector <- function(x, ...){
	x = read_sheet(x)
	def <- as.flow_def(def)
	to_flow(x, def, ...)
}

#' to_flow.list
#' @param desc final flow name
#' @export
to_flow.list <- function(x, def, flowname, flow_run_path, desc,...){
	## --- qobj, missing only works for arguments
	## x is a list of flow_mat, split by jobname
	
	jobs <- lapply(1:nrow(def), function(i, qobj){
		message(".", appendLF = FALSE)
		jobnm = def[i, "jobname"]
		cmds = x[[jobnm]]$cmd; 

		## --- submit def, to get resources for this particular job
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
		if(missing(qobj))
			qobj <- queue(platform = unlist(def2$platform), verbose = FALSE)

		## --- getting defaults of submission and depedency types		
		if(length(d_sub_type) == 0){
			d_sub_type = guess_sub_type(cmds = cmds)
		}
		## guess dep_type
		if(length(d_dep_type) == 0){
			d_dep_type <- guess_dep_type(prev_job = prev_job, cmds = cmds)
		}

		## -------- if cmds are missing; change to echo 0 and make cpu = 1
		cpu = ifelse(cmds[1] == ".", 1, d_cpu)
		## if starts from . echo
		cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1])
		
		jobj = job(q_obj = qobj, name = jobnm, 
						previous_job = prev_job,
						cmds = cmds,
						dependency_type = d_dep_type, 
						submission_type = d_sub_type,
						cpu = d_cpu, queue = d_queue,
						walltime = d_walltime, 
						memory = d_memory)
		return(jobj)
	})
	
	
	fobj <- flow(jobs = jobs,
							 desc = desc, name = flowname,
							 mode = "scheduler", 
		flow_run_path = flow_run_path)

	## --- check if submission or depedency types were guessed
	if(is.null(def$sub_type) | is.null(def$dep_type)){
		message("Submission/definition types were guessed.",
						"\nThis is really a experimental feature.",
						"\nPlease check the following table.",
						"\nIncase of issues please re-submit specifying them explicitly.")
		mydef = create_jobs_mat(fobj)
		cols = c("jobname",  'prev_jobs',  'dep_type', 'sub_type')
		print(knitr::kable(mydef[, cols], col.names=FALSE))
	}

	return(fobj)
}

# def = system.file(package = "flowr", "files/flow_def_ex1.txt")

#' @export
to_flow.data.frame <- function(x, def, 
	grp_col,
	jobname_col,
	cmd_col,
	flowname,
	flow_run_path,
	platform,
	submit = FALSE,
	execute = FALSE,
	qobj, ...){
	
	message("\n\n##--- Getting default values for missing parameters...")
	## --- get defaults sample, job and cmd columns
	if(missing(grp_col)){
		grp_col = "samplename"
		if(grp_col %in% colnames(x))
			message("Using `", grp_col, "` as the grouping column")
		else
			stop("grouping column not specified, and the default 'samplename' is absent in the input x.")
	}
	if(missing(jobname_col)){
		jobname_col = "jobname"
		if(jobname_col %in% colnames(x))
			message("Using `", jobname_col, "` as the jobname column")
		else
			stop("jobname column not specified, and the default 'jobname' is absent in the input x.")
	}
	if(missing(cmd_col)){
		cmd_col = "cmd"
		if(cmd_col %in% colnames(x))
			message("Using `", cmd_col, "` as the cmd column")
		else
			stop("cmd column not specified, and the default 'cmd' is absent in the input x.")
	}
	
	## ---- renaming columns to make it easier for subsequent.
	x[, "jobname"] = x[, jobname_col]
	x[, "cmd"] = x[, cmd_col]
	x[, "samplename"] = x[, grp_col]
	
	
	## --- defaults
	if(missing(flowname)){
		flowname = "flowname"
		message("Using flowname default: ", flowname);
	}
	if(missing(flow_run_path)){
		flow_run_path = getOption("flow_run_path")
		message("Using flow_run_path default: ", flow_run_path);
	}
	
	## --- change all the input columns into character
	x[] <- lapply(x, as.character)
	def[] <- lapply(def, as.character)
	
	message("\n\n##--- Checking flow definition and flow matrix for consistency...")
	def = as.flow_def(def)
	## --- A check x should be in def 
	if(mean(!unique(x$jobname) %in% na.omit(def$jobname))){
		stop("Some jobs in x are not in flow_definition\n")
	}
	## B AND vice-versa
	if(mean(!na.omit(def$jobname) %in% unique(x$jobname))){
		stop("Some jobs in flow_def are not in x\n")
	}
	
	
	## --- Overrides
	message("\n\n##--- Detecting platform...")
	if("platform" %in% colnames(def)){
		message("Will use platform from flow_definition")
	}
	if(!missing(platform)){
		message("Platform supplied, this will override defaults from flow_definition...")
		def$platform = platform
	}
	if(!missing(qobj)){
		message("qobj supplied, this will override defaults from flow_definion OR platform variable")
		message("Use of qobj is depreciated", 
			"Use shell scripts provided here as a template: https://github.com/sahilseth/flowr/tree/master/inst/conf", 
			"You may tweak them and stor in ~/flowr/conf")
	}
	
	message("\n\n##--- flowr submission...")
	samps = unique(x$samplename)
	if(length(samps) > 1){
		message("\n\nDetected ", length(samps), " samples/groups in flow_mat.",
						"\nflow_mat would be split and each would be submitted seperately...")
	}
	
	fobjs <- lapply(samps, function(samp){
		x2 = subset(x, samplename == samp)
		message("\n\nWorking on... ", samp)
		## --- fetch samplename from the flowr_mat
		cmd.list = split.data.frame(x2, x2$jobname)
		desc = paste(flowname, samp, sep = "-")
		fobj = to_flow(x = cmd.list, def = def, 
									 desc = desc,
									 flowname = flowname,
			flow_run_path, 
									 qobj = qobj, ...)
		
		
		### ----- remove THESE !
		if(submit)
			fobjuuid <- submit_flow(fobj, execute = execute)
		if(execute)
			return(fobjuuid)
		else
			return(fobj)
	})
	
	## --- if there is only one sample, fobj is returned
	if(length(fobjs) == 1)
		fobjs = fobjs[[1]]

	return(fobjs)

}

#setMethod("to_flow", signature(x = "list"), definition = .to_flow.list)
#setMethod("to_flow", signature(x = "data.frame"), definition = .to_flow.data.frame)


#' @title to_flow
#' @description Create a \link{flow} object from a list of commands
#' @param x \link{list} of commands
#' @export
cmds_to_flow <- function(cmd.list,
												 samplename = "",
												 infomat,
												 q_obj = queue(type = "lsf", verbose=FALSE),
												 flowname = "stage2",
												 execute = FALSE,
	flow_run_path = "/scratch/iacs/flow_pipe/tmp"){
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
	fobj <- flow(jobs = jobs,
								desc=sprintf("%s-%s", flowname, samplename), name = flowname,
								mode="scheduler", flow_run_path = flow_run_path)
	len = length(jobs)
	#debug(flow:::.submit_flow)
	#mypack:::reload('flow')
	fobj_uuid <- submit_flow(fobj, execute = execute, make_flow_plot = TRUE)
	#   if(sum(flow:::create_jobs_mat(fobj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
	#     cat("Plotting...\n")
	#     try(flow:::plot_flow(x = fobj, type = '1',
	#                           pdf = TRUE, pdffile = file.path(fobj_uuid@flow_path, "flow_design.pdf")))
	#   }
	return(fobj_uuid)
}


