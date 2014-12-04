
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite SQLite
get_connection <- function(path = "/scratch/iacs/iacs_dep/sseth/.flow/flow.sqlite"){
  #   library(sqldf)
  db <- dbConnect(SQLite(), dbname = path)
  return(db)
}









