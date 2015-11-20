## --- this is being archived for now...
# nocov start
if(FALSE){
	get_connection <- function(path = "/rsrch2/iacs/iacs_dep/sseth/rflow/db/main.sqlite"){
		# @importFrom RSQLite dbConnect
		# @importFrom RSQLite SQLite
		#   library(sqldf)
		if (!requireNamespace("pkg", quietly = TRUE)) {
			stop("Pkg needed for this function to work. Please install it.",
				call. = FALSE)
		}
		db <- dbConnect(SQLite(), dbname = path)
		return(db)
	}
}

# nocov end








if(FALSE){
  x = read_sheet("/rsrch2/iacs/ngs_runs/1412_tcga_normals/KIRC/logs/kirc-20150318-12-00-50-f27qz3ZL/flow_details.txt")
  #dbWriteTable(conn = db, name = "tracker", value = x)
  tmp <- dbReadTable(conn = db, name = "tracker")
}
