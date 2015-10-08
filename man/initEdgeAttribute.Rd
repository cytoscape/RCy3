\name{initEdgeAttribute}
\alias{initEdgeAttribute}
\title{
initEdgeAttribute
}
\description{
Create the  edge attribute slot that the Bioconductor graph class
requires, including a default value, and then specifying what the base
type (or 'class') is -- 'char', 'integer', or 'numeric' -- which is needed by
RCytoscape.  This method converts these standard R data type names, to
the forms needed by Cytoscape.
}
\usage{
initEdgeAttribute(graph, attribute.name, attribute.type, default.value)
}

\arguments{
  \item{graph}{a Bioconductor \code{graph} object. }
  \item{attribute.name}{a \code{string}, the name of the new edge
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
  initNodeAttribute
  makeSimpleGraph
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g = new ('graphNEL', edgemode='directed')
  g = initEdgeAttribute (g, 'edgeType', 'char', 'associates with')
}

\keyword{graph}
