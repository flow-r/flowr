
## Adapted from adv- R; hadley wikham
## http://adv-r.had.co.nz/Computing-on-the-language.html
dots <- function(..., .env){
	args = eval(substitute(alist(...)))
	#deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
	args2 = lapply(as.character(args), get, .env)
	names(args2) = args
	return(args2)
}

assert_args_not_null <- function(...,
																 .env = environment()){
	
	args = dots(..., .env = .env)
	lapply(seq_along(args), function(i){
		assert_not_null(args[[i]], names(args)[[i]])
	})
}


#' Assert none of the arguemnts of a function are null.
#'
#' Checks all the arguments in the parent function and makes sure that none of them
#' are NULL
#' 
#' 
#' @param ignore optionally ignore a few variables for checking.
#' @param select optionally only check a few variables of the function.
#' 
#' @export 
#' @details 
#' This function has now been moved to params package.
check_args <- function(ignore, select){
	fn = sys.call(sys.parent())[1]
	env = parent.frame()
	args = ls(env)
	
	if(!missing(ignore))
		args = args[!args %in% ignore]
	
	if(!missing(select))
		args = args[args %in% select]
	
	miss = sapply(args, function(var){
	  
		val = try(get(var, env), silent = TRUE)
		if(is.null(val) | missing(var))
		  return(var)
		else
		  return(NULL)
	})
	miss = unlist(miss) ## vars which are missing
	
	if ( !is.null(miss) ){
		message("Checking arguments for function: ", fn, "\n")
		message("value of following variables is null: ", paste(names(miss), collapse = ", "))
		stop("There are several options to fix this:
				 1. Use opts_flow$set(variable1 = 'value', var1 = 'value') format to define these variables.
				 2. If this function was called directly, you may simply supply these arguments to this function.
				 3. Add these parameters to configuration files.
				 One such example is flowr.conf which resides in ~/flowr/conf folder < or equivalent >. ",
				 "Additionally, options very specific to a single pipeline in may be put in <mypipeline>.conf. ",
				 "The file <mypipeline>.conf sits in the same place as the pipeline itself. ",
				 "Try fetch_pipes() for examples and help(fetch_pipes) for more details.")
	}
	
	lst <- lapply(args, get, env)
	names(lst) = args
	invisible(lst)
}


assert_not_null <- function(x, .varname){
	if (missing(.varname))
		.varname = deparse(substitute(x))
	if (is.null(x))
		stop("Variable ", .varname, " should not be null. You may directly supply it to the function OR add these to ngsflows.conf OR a seperate conf file and use load_opts()")
}


assert_flowdef <- function(x){
	varname = deparse(substitute(x))
	if (!is.flowdef(x)) {
		stop("Variable ", varname, " should be of class flowdef. Use as.flowdef() which performs important checks on the input data.frame")
	}
}

assert_flowmat <- function(x){
	varname = deparse(substitute(x))
	if (!is.flowmat(x)) {
		stop("Variable ", varname, " should be of class flowmat. Use as.flowmat() which performs important checks on the input data.frame")
	}
}

assert_character <- function(x, len){
	varname = deparse(substitute(x))
	if (!is.character(x)) {
		stop("Variable ", varname, " should be of class character.")
	}
	if (!missing(len)) {
		if (length(x) != len) {
			stop("Variable ", varname, " should be of length, ", len)
		}
	}
	
}

#' @importFrom utils compareVersion
assert_version <- function(fobj, min_ver){
	msg = c("This feature is only supported for flows submitted using flowr version: ", min_ver, " and up.")
	ver = try(fobj@version, silent = TRUE)
	
	if(class(ver) == "try-error" | length(ver) == 0)
		stop(msg)
	
	if(compareVersion(ver, min_ver) == -1)
		stop(msg)
	
}



status_cat <- function(x){
	if(x == "") return(0)
	nums = c("created" = 0,
					 "dry-run" = 1,
					 "submitted" = 2,
					 "running" = 3,
					 "exited" = 4,
					 "completed" = 5
	)
	nums[x]
}



assert_status <- function(fobj, status){
	
	msg1 = switch(status,
								"dry-run" = "fobj: no execution details",
								"submitted" = "fobj: not submitted yet",
								"running" = "fobj: is not running",
								"exited" = "fobj: has not exited",
								"completed" = "fobj: has not completed")
								

	msg2 = c("We need a flow with status: ", status, "\n",
					"however, status of this flow is: ", fobj@status)
	
	if(status_cat(fobj@status) < status_cat(status))
		stop(c(msg1, "\n", msg2))

}

