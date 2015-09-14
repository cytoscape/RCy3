\name{noa.names}
\alias{noa.names}
\title{
noa.names
}
\description{
Retrieve the names of the node attributes in the specified graph.
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
  g <- makeSimpleGraph()
  noa.names (g)
    #  [1] "type"  "lfc"   "label" "count"
}

\keyword{graph}
