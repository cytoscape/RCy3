\name{setTooltipInitialDelay}
\alias{setTooltipInitialDelay}
\alias{setTooltipInitialDelay,CytoscapeConnectionClass-method}
\title{setTooltipInitialDelay}
\description{
Specify the number of milliseconds before the tooltip (a small
lightweight window) pops up over a node or edge.
}
\usage{
setTooltipInitialDelay(obj, msecs)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{msecs}{an \code{integer}.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setTooltipDismissDelay,
  setTooltip,
  setNodeTooltipRule,
  setEdgeTooltipRule,
  setNodeTooltipDirect,
  setEdgeTooltipDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'setTooltipInitialDelay demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
    # use node type as the tooltip
  setNodeTooltipRule (cw, 'type')
    # and edgeType
  setEdgeTooltipRule (cw, 'edgeType')
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
     # have the tooltips popup right away, as soon as the mouse hovers
     # over a node or edge, and then stay up as long as the mouse
     # remains on top of that node or edge
  setTooltipInitialDelay (cw, 0)
  setTooltipDismissDelay (cw, 0)
}

\keyword{graph}
