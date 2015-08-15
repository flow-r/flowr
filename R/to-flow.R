
detect_sub_type <- function(cmds){
	sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
	return(sub_type)
}

#' detect_dep_type
#' @param x job object
#' @param cmds a string of commands
#' @param prev_job previous job name
detect_dep_type <- function(x, cmds, prev_job){
	if (length(prev_job) > 1) {
		dep_type = "gather"
	}else if (length(x[[prev_job]]) == 0) {
		dep_type = "none"
	}else if (length(x[[prev_job]]) == length(cmds)) {
		## if same length, serial
		dep_type = "serial"
	}else if (length(x[[prev_job]]) > length(cmds) & length(cmds) == 1  ) {
		## --- if prevous job were more than current
		dep_type = "gather"
	}else if (length(x[[prev_job]]) == 1 & length(cmds) > 1) {
		## previous was only one, and current are a few
		dep_type = "burst"
	}
	return(dep_type)
}




#' @title
#' Create flow objects
#'
#' @description
#' Use a set of shell commands and flow definiton to create \link{flow} object.
#'
#' @param x path (char. vector) to flow_mat, a data.frame or a list.
#' @param def A flow definition table. Basically a table with resource requirements and mapping of the jobs in this flow
#' @param platform character vector, specifying the platform to use. local, lsf, torque, moab, sge, slurm, ...
#' This over-rides the platform column in flowdef.
#' @param grp_col column name used to split x (flow_mat). Default: `samplename`
#' @param jobname_col column name with job names. Default: `jobname`
#' @param cmd_col column name with commands. Default: `cmd`
#' @param flowname name of the flow
#' @param flow_run_path Path to a folder. Main operating folder for this flow. Default it `get_opts("flow_run_path")`.
#' @param desc Advanced Use. final flow name, please don't change.
#'
#' @param ... Supplied to specific functions like \link{to_flow.data.frame}
#'
#' @param submit Depreciated. Use submit_flow on flow object this function returns. TRUE/FALSE
#' @param execute Depreciated. Use submit_flow on flow object this function returns. TRUE/FALSE, an paramter to submit_flow()
#' @param qobj Depreciated, modify \href{http://docs.flowr.space/en/latest/rd/vignettes/build-pipes.html#cluster-interface}{cluster templates} instead.  A object of class \link{queue}.
#'
#' @examples
#' ex = file.path(system.file(package = "flowr"), "pipelines")
#' flowmat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
#' flowdef = as.flowdef(file.path(ex, "sleep_pipe.def"))
#' fobj = to_flow(x = flowmat, def = flowdef, flowname = "sleep_pipe", platform = "lsf")
#'
#'
#' @details The parameter x can be a path to a flow_mat, or a data.frame (as read by read_sheet).
#' This is a minimum three column matrix with three columns: samplename, jobname and cmd
#'
#'
#'
#' @return
#' Returns a flow object. If execute=TRUE, fobj is rich with information about where and how
#' the flow was executed. It would include details like jobids, path to exact scripts run etc.
#' To use kill_flow, to kill all the jobs one would need a rich flow object, with job ids present.
#'
#' \subsection{Behaviour:}{
#' What goes in, and what to expect in return?
#' \itemize{
#' \item submit=FALSE & execute=FALSE: Create and return a flow object
#' \item submit=TRUE & execute=FALSE: dry-run, Create a flow object then, create a structured execution folder with all the commands
#' \item submit=TRUE, execute=TRUE: Do all of the above and then, submit to cluster
#' }
#' }
#'
#' @export
to_flow <- function(x, ...) {
	#message("input x is ", class(x))
	UseMethod("to_flow")
	warnings()
}

#' @rdname to_flow
#' @export
to_flow.vector <- function(x, def,
	grp_col,
	jobname_col,
	cmd_col,
	...){

	x = as.flowmat(x, grp_col, jobname_col, cmd_col)
	to_flow(x, def, ...)

}

#' @rdname to_flow
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

	## --- change all the input columns into character
	x[] <- lapply(x, as.character)
	x = as.flowmat(x,
								 grp_col = grp_col,
								 jobname_col = jobname_col,
								 cmd_col = cmd_col)
	def = as.flowdef(def)

	## --- defaults
	if (missing(flowname)) {
		flowname = "flowname"
		message("Using flowname default: ", flowname);
	}
	if (missing(flow_run_path)) {
		flow_run_path = get_opts("flow_run_path")
		message("Using flow_run_path default: ", flow_run_path);
	}


	message("\n##--- Checking flow definition and flow matrix for consistency...")

	## ---  COMPARE flowdef and flowmat jobnames
	msg = c("\nflowdef jobs: ", paste(def$jobname, collapse = " "),
					"\nflowmat jobs: ", paste(unique(x$jobname), collapse = " "))
	if (mean(!unique(x$jobname) %in% na.omit(def$jobname))) {
		stop("Some jobs in x are not in flow definition\n", msg)
	}
	if (mean(!na.omit(def$jobname) %in% unique(x$jobname))) {
		stop("Some jobs in flowdef are not in flowmat\n", msg)

	}


	## --- Overrides
	message("\n##--- Detecting platform...")
	if ("platform" %in% colnames(def)){
		message("Will use platform from flow definition")
	}
	if (!missing(platform)){
		message("Platform supplied, this will override defaults from flow definition...")
		def$platform = platform
	}
	if (!missing(qobj)){
		message("qobj supplied, this will override defaults from flow_definion OR platform variable")
		message("Use of qobj is depreciated",
			"Use shell scripts provided here as a template: https://github.com/sahilseth/flowr/tree/master/inst/conf",
			"You may tweak them and stor in ~/flowr/conf")
	}

	samps = unique(x$samplename)
	if (length(samps) > 1)
		message("Detected multiple samples. Would containerize this submission.")

	fobjs <- lapply(samps, function(samp){
		x2 = subset(x, x$samplename == samp)
		message("\nWorking on... ", samp)
		## --- fetch samplename from the flowr_mat
		cmd.list = split.data.frame(x2, x2$jobname)
		desc = paste(flowname, samp, sep = "-")
		fobj = to_flow(x = cmd.list,
			def = def,
			desc = desc,
			flowname = flowname,
			flow_run_path = flow_run_path,
			qobj = qobj, ...)


		### ----- remove THESE !
		if (execute|submit){
			message("\n##--- flowr submission...")
			message("\nYou may use fobj <- to_flow(...); submit_flow(fobj)")
			if (length(samps) > 1){
				message("\n\nDetected ", length(samps), " samples/groups in flow_mat.",
					"\nflow_mat would be split and each would be submitted seperately...")
			}
			if (execute)
				submit = TRUE
		}

		if (submit)
			fobjuuid <- submit_flow(fobj, execute = execute)
		if (execute)
			return(fobjuuid)
		else
			return(fobj)
	})

	## --- if there is only one sample, fobj is returned, else a list of flow objects
	if (length(fobjs) == 1)
		fobjs = fobjs[[1]]

	invisible(fobjs)
}



#' @rdname to_flow
#' @importFrom utils packageVersion
#' @export
to_flow.list <- function(x, def, flowname, flow_run_path, desc, qobj, ...){
	## --- qobj, missing only works for arguments
	## x is a list of flow_mat, split by jobname

	jobs <- lapply(1:nrow(def), function(i, qobj){
		message(".", appendLF = FALSE)
		jobnm = def[i, "jobname"]
		cmds = x[[jobnm]]$cmd;

		## --- submit def, to get resources for this particular job
		def2 = subset(def, def$jobname == jobnm)
		prev_job = unlist(def2$prev_jobs) ## SHOULD be NA
		if (!is.na(prev_job))
			prev_job = strsplit(prev_job, ",")[[1]] ## supports multi
		d_cpu = unlist(def2$cpu_reserved)
		d_walltime = unlist(def2$walltime)
		d_memory = as.character(unlist(def2$memory_reserved))
		d_queue = unlist(def2$queue)
		d_dep_type = unlist(def2$dep_type)
		d_sub_type = unlist(def2$sub_type)
		d_nodes = unlist(def2$nodes)
		d_jobid = unlist(def2$jobid)

		if (missing(qobj))
			qobj <- queue(platform = unlist(def2$platform), verbose = FALSE)

		## --- getting default for nodes
		if (is.null(d_nodes))
			d_nodes = '1'


		## -------- if cmds are missing; change to echo 0 and make cpu = 1
		cpu = ifelse(cmds[1] == ".", 1, d_cpu)
		## if starts from . echo
		cmds[1] = ifelse(cmds[1] == "\\.", "echo done", cmds[1])

		jobj = job(q_obj = qobj,
			name = jobnm,
			jobname = sprintf("%03d.%s", d_jobid, jobnm),
			previous_job = prev_job,
			cmds = cmds,
			dependency_type = d_dep_type,
			submission_type = d_sub_type,
			cpu = d_cpu, queue = d_queue,
			walltime = d_walltime,
			nodes = d_nodes,
			memory = d_memory)
		return(jobj)
	})

	fobj <- flow(jobs = jobs,
		desc = desc, name = flowname,
		mode = "scheduler",
		version = as.character(packageVersion("flowr")),
		flow_run_path = flow_run_path)

	## --- check if submission or depedency types were guessed
	if (is.null(def$sub_type) | is.null(def$dep_type)){
		message("Submission/definition types were guessed.",
			"\nThis is really a experimental feature.",
			"\nPlease check the following table.",
			"\nIncase of issues please re-submit specifying them explicitly.")
		mydef = create_jobs_mat(fobj)
		cols = c("jobname",  'prev_jobs',  'dep_type', 'sub_type')
		print(knitr::kable(mydef[, cols], col.names=FALSE))
	}

	invisible(fobj)
}






















## --------------------- d e p r e c i a t e d        f u n c t i o n s ----------------------------- ##




#setMethod("to_flow", signature(x = "list"), definition = .to_flow.list)
#setMethod("to_flow", signature(x = "data.frame"), definition = .to_flow.data.frame)


#' @title cmds_to_flow: DEPRECIATED
#' @description Create a \link{flow} object from a list of commands
#'
#' @param cmd.list list of commands
#' @param samplename name of the sample
#' @param infomat flowdef
#' @param q_obj queue object
#' @param flowname name of the flow
#' @param execute TRUE/FALSE
#' @param flow_run_path outpath
#'
#' @export
cmds_to_flow <- function(cmd.list,
	samplename = "",
	infomat,
	q_obj = queue(type = "lsf", verbose=FALSE),
	flowname = "stage2",
	execute = FALSE,
	flow_run_path = "/scratch/iacs/flow_pipe/tmp"){
	.Deprecated("to_flow")
	## trim down the list
	cmd.list = lapply(cmd.list, function(y) Filter(function(x) !x == "", y))
	infomat$dep_type = ifelse(infomat$previous_job==".", "none", "serial")
	#infomat$previous_jobs = ifelse(infomat$previous_job==".", NULL, infomat$previous_job) ## prev job null
	#infomat <- cbind(jobnames, sub_type, cpus, prev_jobs, dep_type)
	## Error handling: missing in info
	missing.info = names(cmd.list)[!names(cmd.list) %in% infomat$jobname]
	if (length(missing.info) > 0){
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
	if (length(missing.prev) > 0)
		stop("\n\nMessage:\nOops issue with infomat.\n",
			"It seems these previous job names are missing from the jobname column:\n",
			"Dependencies are only supported within the same flow.\n",
			missing.prev, "\n")
	if ( mean(table(infomat$jobname)) != 1 || mean(table(names(cmd.list))) != 1 )
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
		if (length(sub_type) == 0)
			sub_type = as.character(ifelse(length(cmds) > 1, "scatter", "serial"))
		## guess dep_type
		if (length(prev_job) > 1){
			dep_type = "gather"
		}else if (length(cmd.list[[prev_job]]) == 0){
			dep_type = "none"
		}else if (length(cmd.list[[prev_job]]) == length(cmds) ){ ## if same length, serial
			dep_type = "serial"
		}else if (length(cmd.list[[prev_job]]) > length(cmds) & length(cmds) == 1  ){ ## if same length, serial
			dep_type = "gather"
		}else if (length(cmd.list[[prev_job]]) == 1 & length(cmds) > 1){ ## if same length, serial
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
	#   if (sum(flow:::create_jobs_mat(fobj)$prev_jobs != ".") > 2){ ## at least 0.1some have dep.
	#     cat("Plotting...\n")
	#     try(flow:::plot_flow(x = fobj, type = '1',
	#                           pdf = TRUE, pdffile = file.path(fobj_uuid@flow_path, "flow_design.pdf")))
	#   }
	return(fobj_uuid)
}



