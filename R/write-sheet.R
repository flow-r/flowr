
#' A simple wrapper around write.table with some sugar.
#' @export
write_sheet <- function(x, file){
	write.table(x = x, file = file, sep = "\t", row.names = FALSE, quote = FALSE)
}
