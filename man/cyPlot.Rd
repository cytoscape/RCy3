\name{cyPlot}
\alias{cyPlot}
\title{cyPlot}
\description{
Given a node attribute data frame (node.df) with the node names in column 1, 
and an edge attribute data.frame (edge.df) with node names in the first two columns,
cyPlot creates a graphNEL object with nodes, edges, and their attributes 
that can be loaded into Cytoscape with CytoscapeWindow. 
}
\usage{
cyPlot(node.df, edge.df)
}
\arguments{
  \item{node.df}{a \code{data.frame} with node names in the first column. }
  \item{edge.df}{a \code{data.frame} with node names in the first two columns. }
}

\value{
a graphNEL object with nodes, edges, and their attributes
}
\author{Mark Grimes}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

	# Create node and edge data frames
  node.tbl <- data.frame(Gene.Name=c("FOX", "CENT", "RTK"), Numeric.Data=as.numeric(c(10, 12, 7)), nodeType=c("ts factor", "nuclear porin", "receptor"))
  edge.tbl <- data.frame(Gene.1=c("FOX", "FOX", "CENT", "CENT"), Gene.2=c("CENT", "RTK", "FOX", "RTK"), Weight=as.numeric(c(0.1, 1, 2, 0.02)), edgeType="interaction")
  window.name <- 'demo cyPlot'
	# Create graph and send to Cytoscape
  cg <- cyPlot(node.tbl, edge.tbl)
  cw <- CytoscapeWindow (window.name, graph=cg)
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)
}

\keyword{graph}
