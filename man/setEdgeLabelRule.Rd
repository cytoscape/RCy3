\name{setEdgeLabelRule}
\alias{setEdgeLabelRule}
\alias{setEdgeLabelRule,CytoscapeWindowClass-method}
\title{setEdgeLabelRule}
\description{
Specify the edge attribute to be used as the label displayed on  each edge.
Non-character attributes are converted to strings before they are used.}


\usage{
setEdgeLabelRule(obj, edge.attribute.name)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
        rule is applied, determine the edgeLabel on each edge.}
}

\value{
None.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  setEdgeLabelRule (cw, 'edgeType')
}

\keyword{graph}
