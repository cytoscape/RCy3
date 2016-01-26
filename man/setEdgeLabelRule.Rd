\name{setEdgeLabelRule}
\alias{setEdgeLabelRule}
\alias{setEdgeLabelRule,CytoscapeWindowClass-method}
\title{setEdgeLabelRule}
\description{
Specify the edge attribute to be used as the label displayed on each edge.
Non-character attributes are converted to strings before they are used.}

\usage{
setEdgeLabelRule(obj, edge.attribute.name)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will
        determine the edge label on each edge, when this rule is applied.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeShapeRule (detailed example)
  setEdgeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setEdgeLabelRule (cw, 'edgeType')
}

\keyword{graph}
