
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
check_args <- function(){
  fn = sys.call(sys.parent())[1]
  env = parent.frame()
  args = ls(env)
  needtoexit = FALSE
  for (i in 1:length(args)) {
    var = args[[i]]
    val = get(var, env)
    if (is.null(val)) {
      message("Checking arguments for function: ", fn, ", value of '", var, "' is null.")
      needtoexit = TRUE
    }
  }
  if (needtoexit) stop("Add these parameters to ngsflows.conf.")
}


assert_not_null <- function(x, .varname){
  if (missing(.varname))
    .varname = deparse(substitute(x))
  if (is.null(x))
    stop("Variable ", .varname, " should not be null. You may directly supply it to the function OR add these to ngsflows.conf OR a seperate conf file and use load_conf()")
}


assertFlowdef <- function(x){
	varname = deparse(substitute(x))
	if (!is.flowdef(x)) {
		stop("Variable ", varname, " should be of class flowdef. Use as.flowdef() which performs important checks on the input data.frame")
	}
}

assertFlowmat <- function(x){
	varname = deparse(substitute(x))
	if (!is.flowmat(x)) {
		stop("Variable ", varname, " should be of class flowmat. Use as.flowmat() which performs important checks on the input data.frame")
	}
}


