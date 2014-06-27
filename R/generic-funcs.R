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
