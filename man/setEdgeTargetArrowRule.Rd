\name{setEdgeTargetArrowRule}
\alias{setEdgeTargetArrowRule}
\alias{setEdgeTargetArrowRule,CytoscapeWindowClass-method}
\title{specify the arrow types to be used at the end of an edge, at the
  'target' node}
\description{
Specify how data attributes -- for the specified named attribute -- are
mapped to the target arrow type.}

\usage{
setEdgeTargetArrowRule(obj, edge.attribute.name, attribute.values, arrows, default='ARROW')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will determine
        the target arrow of each edge when this rule is applied.}
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
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTargetArrowRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  
  arrows <- c('DIAMOND', 'ARROW', 'CIRCLE')
  edgeType.values <- c('phosphorylates', 'synthetic lethal', 'undefined')
  
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)
}

\keyword{graph}
