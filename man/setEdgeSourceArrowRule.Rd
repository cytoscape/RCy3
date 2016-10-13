\name{setEdgeSourceArrowRule}
\alias{setEdgeSourceArrowRule}
\alias{setEdgeSourceArrowRule,CytoscapeWindowClass-method}
\title{specify the arrow types to be used at the end of an edge at the
  'source' node}
\description{
Specify how data attributes -- for the specified named attribute -- are
mapped to the source arrow type.}

\usage{
setEdgeSourceArrowRule(obj, edge.attribute.name, attribute.values, arrows, default='ARROW')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will determine
        the source arrow of each edge when this rule is applied.}
  \item{attribute.values}{A list of scalar, discrete values. For instance,
  interaction types:'phosphorylates', 'ubiquinates', 'represses', 'activates' }
  \item{arrows}{One arrow type for each of the attribute.values}
  \item{default}{The arrow type to use when an explicit mapping is not provided.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  \code{\link{getArrowShapes}}
  setNodeBorderColorRule (detailed example)
  setEdgeColorRule
  setNodeShapeRule
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeSourceArrowRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  arrows <- c('Arrow', 'Diamond', 'Circle')
  edgeType.values <- c('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, arrows)
}

}
\keyword{graph}
