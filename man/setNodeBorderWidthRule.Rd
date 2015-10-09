\name{setNodeBorderWidthRule}

\alias{setNodeBorderWidthRule}
\alias{setNodeBorderWidthRule,CytoscapeWindowClass-method}

\title{setNodeBorderWidthRule}

\description{
Specify the node attribute which controls the thickness of the nodes
displayed in the graph.  This is currently only a lookup mapping.  An
interpolated mapping will be added in the future.
}

\usage{
setNodeBorderWidthRule(obj, node.attribute.name, attribute.values, line.widths, default.width)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will, when this
        rule is applied, determine the nodeBorderWidth on each node.}
  \item{attribute.values}{observed values of the specified attribute on the nodes.}
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

  cw <- CytoscapeWindow ('setNodeBorderWidthRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  node.attribute.values = c ('kinase', 'transcription factor', 'glycoprotein')
  line.widths = c (0, 8, 16)
  setNodeBorderWidthRule (cw, 'type', node.attribute.values, line.widths)
}

\keyword{graph}
