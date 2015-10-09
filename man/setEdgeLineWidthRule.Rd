\name{setEdgeLineWidthRule}

\alias{setEdgeLineWidthRule}
\alias{setEdgeLineWidthRule,CytoscapeWindowClass-method}

\title{setEdgeLineWidthRule}

\description{
Specify the edge attribute which controls the thickness of the edges
displayed in the graph.  This is currently only a lookup mapping.  An
interpolated mapping will be added in the future.
}

\usage{
setEdgeLineWidthRule(obj, edge.attribute.name, attribute.values, line.widths, default.width)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
        rule is applied, determine the edgeLineWidth on each edge.}
  \item{attribute.values}{observed values of the specified attribute on the edges.}
  \item{line.widths}{the corresponding widths.}
  \item{default.width}{use this where the rule fails to apply}
}


\value{
None.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLineWidthRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  edge.attribute.values = c ('phosphorylates', 'synthetic lethal',   'undefined')
  line.widths = c (0, 8, 16)
  setEdgeLineWidthRule (cw, 'edgeType', edge.attribute.values, line.widths)
}

\keyword{graph}
