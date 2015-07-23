


is.flowmat <- function(x){
	class(x)[1] == "flowmat"
}

## add to flowr:
#' @title
#' Taking in a named list and returns a two columns data.frame
#' @param x a named list OR vector. Where name corresponds to the jobname and value is a vector of commands to run
#' @export
to_flowmat <- function(x, ...) {
	UseMethod("to_flowmat")
}

#' @rdname to_flowmat
#' @export
as.flowmat = to_flowmat


#' @rdname to_flowmat
#' @export
to_flowmat.list <- function(x, samplename){
	if(missing(samplename))
		stop("to_flowmat needs a samplename !")

	if(is.null(names(x)))
		stop("supply a named object to to_flowmat")

	ret <- lapply(1:length(x), function(i){
		cmd = x[[i]]
		jobname = names(x[i])
		data.frame(jobname, cmd, stringsAsFactors = FALSE)
	})
	ret = do.call(rbind, ret)
	ret = cbind(samplename = samplename, ret)
	attr(ret, "class") <- c("flowmat", "data.frame")
	return(ret)

}

#' @rdname to_flowmat
#' @export
to_flowmat.data.frame <- function(x){
	attr(x, "class") <- c("flowmat", "data.frame")
	message("Looks good, just check...")
	return(x)
}

## not used, use char instead
to_flowmat.character <- function(x){
	.Deprecated("to_flowmat.list", msg = "Supply a named list instead of a vector.")
	ret <- lapply(1:length(x), function(i){
		cmd = x[i]
		jobname = names(x[i])
		data.frame(jobname, cmd, stringsAsFactors = FALSE)
	})
	do.call(rbind, ret)
}


#' @rdname to_flowmat
#' @export
to_flowmat.flow <- function(x){
	## -- get all the commands as rows
	lst = lapply(x@jobs, function(y){
		cmd = list(y@cmds)
		names(cmd) = y@name
		to_flowmat(x = cmd, samplename = x@desc)
	})
	mat = do.call(rbind, lst)
	return(mat)
}


