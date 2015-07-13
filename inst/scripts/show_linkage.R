showlinkage <- function(df, package, suggests=FALSE, task=NULL, include.self=TRUE){
	require(igraph)
	
	tasks <- x[(ncol(df)-28):ncol(df)]
	df <- df[1:(ncol(df)-29)]
	
	if (!suggests){
		df[] <- lapply(df, function(x){replace(x, x == 2, 0)}) 
	} else {
		df[] <- lapply(df, function(x){replace(x, x == 2, 1)}) 
	}
	
	if (!is.null(task)){
		task <- paste("taskclass", task, sep="")
		rc <<- which(tasks[which(colnames(tasks)==task)]==1)
		df <- df[rc, rc]
	} 
	
	if (package != "ByTask"){
		df <- df1==1 | rownames(df)==package), which(df[which(rownames(df)==package), ]==1)), c(which(df[package]==1 | rownames(df)==package), which(df[which(rownames(df)==package), ]==1))]
if (!include.self){
	df <- df[-which(rownames(df)==package), -which(colnames(df)==package)]
}
	}
	
	df <- t(df) # reverse linkage arrows
	
	g <- graph.adjacency(df, mode="directed", weighted=TRUE, add.rownames=TRUE)
	
	plot(g, vertex.label.color="blue",
		edge.width=.5, edge.color="#ACC49D", edge.arrow.size=.5,
		vertex.label.cex=.7, vertex.label=row.names(df), vertex.size=0, vertex.color="white",
		#layout=layout.reingold.tilford
		layout=layout.kamada.kawai
	)
}