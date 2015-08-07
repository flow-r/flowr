
as.c=as.character
as.n=as.numeric

slots_as_list <- function(x, names=slotNames(x)){
    ret <- lapply(names, function(n) try(slot(x, n)))
    names(ret) = names
    return(ret)
}

#setMethod("as.list", signature=c(x="queue"), definition=f)

#' replace slots in a S4 object
#' @param object a S4 object
#' @param ... set of slot names to be replaced. This needs to be a named vector
#' @keywords internal
#' @import methods
replace_slots <- function(object, ...){
    args <- as.list(match.call(expand.dots=TRUE))
    args <- args[names(args) %in% methods::slotNames(class(object))]
    for(s in names(args)){
        slot(object,s) <- args[[s]]
    }
    return(object)
}

#' @title get_unique_id
#' @description get_unique_id
#' @param random_length Integer, defaults to 8. In our opinion 8 serves well, providing 'uniqueness' and not being much of a eyesore.
#' @param prefix Default \code{id}. Character string to be added in the front.
#' @param suffix Default ''. Character string to be added in the end.
#' @export
#' @keywords internal
#' @examples \dontrun{
#' get_unique_id(base = id, random_length = 8)}
get_unique_id <- function(prefix="id", suffix = "", random_length = 8){
  tm = format(Sys.time(), "%Y%m%d-%H-%M-%S")
  uid = paste(as.character(sample(c(letters, toupper(letters), 0:9), size = random_length)), collapse = "")
  ret <- sprintf("%s-%s-%s%s", prefix, tm, uid, suffix)
  return(ret)
}





jobnames <- function(x){
	sapply(x@jobs, slot, 'name')
}

cmds <- function(x){
	sapply(x@jobs, slot, 'cmds')
}


