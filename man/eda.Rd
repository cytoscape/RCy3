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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}
\seealso{
 eda.names
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g  <- makeSimpleGraph()
  eda (g, 'edgeType')
    #             A|B                B|C                C|A
    # "phosphorylates" "synthetic lethal"        "undefined"

}
\keyword{graph}
