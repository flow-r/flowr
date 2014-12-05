
as.c=as.character; as.n=as.numeric

slots_as_list <- function(x, names=slotNames(x)){
    ret <- lapply(names,function(n) slot(x, n))
    names(ret) = names
    return(ret)
}

#setMethod("as.list", signature=c(x="queue"), definition=f)

replace_slots <- function(object, ...){
    args <- as.list(match.call(expand.dots=TRUE))
    args <- args[names(args) %in% slotNames(class(object))]
    for(s in names(args)){
        slot(object,s) <- args[[s]]
    }
    return(object)
}

#' @title get_unique_id
#' @description get_unique_id
#' @param base
#' @param random_length
#' @param prefix ="id"
#' @param suffix
#' @export
#' @examples \dontrun{
#' get_unique_id(base = id, random_length = 8)}
get_unique_id <- function(prefix="id", suffix = "", random_length = 8){
  tm = format(Sys.time(), "%Y%m%d-%H-%M-%S")
  uid = paste(as.character(sample(c(letters, toupper(letters), 0:9), size = random_length)), collapse = "")
  ret <- sprintf("%s-%s-%s%s", prefix, tm, uid, suffix)
  return(ret)
}