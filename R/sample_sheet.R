read_sample_sheet <- function(x, id_column = "sample_id"){
  ext <- tools:::file_ext(x)
  if(ext %in% c("tsv", "txt")){
    mat <- read.table(x, as.is=TRUE, sep="\t", header=TRUE, stringsAsFactors = FALSE,
                      comment.char = '#', strip.white=TRUE, blank.lines.skip=TRUE, quote = "")
  }else if(ext=="csv"){
    mat <- read.csv2(x, as.is=TRUE, sep=",", header=TRUE, stringsAsFactors = FALSE,
                     comment.char = '#', strip.white=TRUE, blank.lines.skip=TRUE, quote = "")
  }
  else if(ext=="xlsx"){
    library(xlsx)
    mat <- xlsx:::read.xlsx2(file = x, sheetName = "sample_sheet", startRow = 2, stringsAsFactors = FALSE)
  }
  else{
    stop("Sorry we do not recognize this file format", ext, "please use tsv, csv or xlsx2 (sheetname: sample_sheet)")
  }
  ### ------ remove blank rows and columns
  mat <- mat[!mat[, id_column] %in% c("", NA), !grepl("^X", colnames(mat))]
  check_fastq_sheet(mat, id_column = id_column)
  return(mat)
}