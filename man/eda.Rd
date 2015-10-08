\name{eda}
\alias{eda}
\title{
eda
}
\description{
Obtain the value of the specified edge attribute for every edge in the graph.
}
\usage{
eda(graph, edge.attribute.name)
}
\arguments{
  \item{graph}{typically, a bioc graphNEL object}
  \item{edge.attribute.name}{a character string}
}
\details{
The edge.attribute.name may be obtained from the function, eda.names.
}
\value{
A list, the contents of which are the attribute values, the names of
which are the names of the edges.
}
\author{
Paul Shannon
}
\seealso{
 eda.names
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

g  <- makeSimpleGraph()
eda (g, 'edgeType')

## The function is currently defined as
function (graph, edge.attribute.name)
{
  unlist (sapply (names (edgeData (graph)), function (n) edgeData (graph)[[n]][[edge.attribute.name]]))

  } # eda
}
\keyword{graph}
