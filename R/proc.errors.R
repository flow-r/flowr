#' @title Error Handler
#' @description function to handle all error descriptions
#' @keywords internal
#' @param x this is a string(s) with short error summary
error <- function(x){
  if("no.flow.det.file" %in% x){
    y = "Looks like flow_details.txt does not exists in flow path. This could happen due to multiple reasons. One of them being flow was not completely submitted."
  }
  return(y)
}