
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


#' checks all the arguments in the parent frame.
#' None of them should be null.
#' 
#' @description 
#' 
#' This function may be optionally moved to a more generic package.
#' 
#' @param ignore optionally ignore a few variables for checking.
#' @param select optionally only check a few variables of the function.
#' @export
check_args <- function(ignore, select){
	fn = sys.call(sys.parent())[1]
	env = parent.frame()
	args = ls(env)
	
	if(!missing(ignore))
		args = args[!args %in% ignore]
	
	if(!missing(select))
		args = args[args %in% select]
	
	miss = sapply(args, function(var){
		val = get(var, env)
		if(is.null(val))
		  return(var)
		else
		  return(NULL)
	})
	miss = unlist(miss) ## vars which are missing
	
	if ( !is.null(miss) ){
		message("Checking arguments for function: ", fn, "\n")
		message("value of following variables is null: '", paste(names(miss), collapse = ", "))
		stop("There are several options to fix this:
				 1. Use set_opts(variable1 = 'value', var1 = 'value') format to define these variables.
				 2. If this function was called directly, you may simply supply these arguments to this function.
				 3. Add these parameters to configuration files.
				 Two such examples are: ngsflows.conf and flowr.conf which reside in ~/flowr/conf folder < or equivalent >. ",
				 "Typically one would add generic options in flowr.conf and ngs specific options in ngsflows.conf. ", 
				 "Additionally, options very specific to a single pipeline in may be put in <mypipeline>.conf. ",
				 "The file <mypipeline>.conf sits in the same place as the pipeline itself. ",
				 "Try fetch_pipes() for examples and help(fetch_pipes) for more details.")
	}
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

