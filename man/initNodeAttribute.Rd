\name{initNodeAttribute}
\alias{initNodeAttribute}
\title{
initNodeAttribute
}
\description{
Create the  node attribute slot that the Bioconductor graph class
requires, including a default value, and then specifying what the base
type (or 'class') is -- 'char', 'integer', or 'numeric' -- which is needed by
RCytoscape.  This method converts these standard R data type names, to
the forms needed by Cytoscape.
}
\usage{
initNodeAttribute(graph, attribute.name, attribute.type, default.value)
}

\arguments{
  \item{graph}{a Bioconductor \code{graph} object. }
  \item{attribute.name}{a \code{string}, the name of the new node
    attribute.}
  \item{attribute.type}{a \code{string}, either 'char', 'integer', or 'numeric'}
  \item{default.value}{something sensible, of the right type}
}

\value{
Returns the modified graph.
}
\author{
Paul Shannon
}
\seealso{
  initEdgeAttribute
  makeSimpleGraph
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g = new ('graphNEL', edgemode='directed')
  g = initNodeAttribute (g, 'lfc', 'numeric', 1.0)
}
\keyword{graph}
