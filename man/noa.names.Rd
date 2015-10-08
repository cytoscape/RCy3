\name{noa.names}
\alias{noa.names}
\title{
noa.names
}
\description{
Retrieve the names of the node attributes in the specified graph.
These are typically strings like 'type', 'label', 'count', and
(strongly recommended when you create a graph) 'nodeType'.  Once you are
reminded of the names of the edge attributes, you can use the method
'eda' to get all the values of this attribute for the edges in the graph.
}
\usage{
noa.names(graph)
}
\arguments{
  \item{graph}{typically, a bioc graphNEL)}
}
\value{
A list, the contents of which are the attribute values, the names of
which are the names of the edges.
}
\author{
Paul Shannon
}

\seealso{
  noa, eda, eda.names
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g <- makeSimpleGraph()
  noa.names (g)
    #  [1] "type"  "lfc"   "label" "count"
}

\keyword{graph}
