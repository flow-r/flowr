

#' read_sheet
#' @description Auto detect file type, read it and clean it.
#' @param x
#' @param id_column all rows which have this column as blank are skipped. See details.
#' @param ... passed onto \link{read.xlsx}, read.table of read.csv2 depending on the file type.
#' @details 
#' If id_column is skipped the first column takes its place.
#' @importFrom openxlsx read.xlsx
#' @importFrom tools file_ext
#' @export
read_sheet <- function(x, id_column, start_row = 1, sheet = "sample_sheet", ext, ...){
	if(missing(ext))
		ext <- file_ext(x)
	if(ext %in% c("tsv", "txt", "conf")){
		mat <- read.table(x, as.is=TRUE, sep="\t", header=TRUE, stringsAsFactors = FALSE,
			quote = "",
			comment.char = '#', strip.white=TRUE, blank.lines.skip=TRUE, ...)
	}else if(ext=="csv"){
		mat <- read.csv2(x, as.is=TRUE, sep=",", header=TRUE, stringsAsFactors = FALSE,
			quote = "",
			comment.char = '#', strip.white=TRUE, blank.lines.skip=TRUE, ...)
	}
	else if(ext=="xlsx"){
		mat <- read.xlsx(x, sheet = sheet, startRow = start_row, ...)
	}
	else{
		cat("Sorry we do not recognize this file format", ext, "please use tsv, csv or xlsx2 (sheetname: sample_sheet)")
	}
	### ------ remove blank rows and columns
	if(missing(id_column)) {
		id_column = 1
		message("Reading file, using '", colnames(mat)[id_column], "' as id_column to remove empty rows.");
		}
	mat <- mat[!mat[, id_column] %in% c("", NA), !grepl("^X", colnames(mat))]
	return(mat)
}
