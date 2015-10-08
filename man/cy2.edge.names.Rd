\name{cy2.edge.names}
\alias{cy2.edge.names}
\title{
cy2.edge.names
}
\description{
Bioconductor graph edges are named, i.e., A~B.  The same edge in the
Cytoscape domain would be 'A (<edgeType>) B', where '<edgeType>' might
be 'phosphorylates' or 'represses'.
}
\usage{
cy2.edge.names(graph, R.edge.names=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{An R graph}
  \item{R.edge.names}{one or more R graph-style edge names.  default NA,
    in which case all edges in the graph are translated to cy2-style.}
  
}

\value{
A named list, in with Cytoscape edges names are the content, and bioc
graph edge names are their names.
}
\author{
Paul Shannon
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g <- makeSimpleGraph ()
  cy2.edge.names (g)
    #                      A~B                      B~C                      C~A 
    #    "A (phosphorylates) B" "B (synthetic lethal) C"        "C   (undefined) A" 
  cy2.edge.names (g, R.edge.names="B~C")
    #                      B~C 
    # "B (synthetic lethal) C" 

}

\keyword{graph}%
