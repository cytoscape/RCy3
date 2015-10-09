\name{eda.names}
\alias{eda.names}
\title{
eda.names
}
\description{
Retrieve the names of the edge attributes in the specified graph.
These are typically strings like 'score', 'weight', 'link', and
(strongly recommended when you create a graph) 'edgeType'.  Once you are
reminded of the names of the edge attributes, you can use the method
'eda' to get all the values of this attribute for the edges in the graph.
}
\usage{
eda.names (graph)
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
 noa, eda, noa.names
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g <- makeSimpleGraph()
  eda.names (g)
   # "edgeType" "score"    "misc"
}

\keyword{graph}
