
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite SQLite
get_connection <- function(path = "/rsrch2/iacs/iacs_dep/sseth/rflow/db/main.sqlite"){
  #   library(sqldf)
  db <- dbConnect(SQLite(), dbname = path)
  return(db)
}








if(FALSE){
  x = read.table("/rsrch2/iacs/ngs_runs/1412_tcga_normals/KIRC/logs/kirc-20150318-12-00-50-f27qz3ZL/flow_details.txt", "\t", header = TRUE)
  #dbWriteTable(conn = db, name = "tracker", value = x)  
  tmp <- dbReadTable(conn = db, name = "tracker")
}
