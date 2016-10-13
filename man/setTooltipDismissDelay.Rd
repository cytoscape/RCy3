\name{setTooltipDismissDelay}
\alias{setTooltipDismissDelay}
\alias{setTooltipDismissDelay,CytoscapeConnectionClass-method}
\title{setTooltipDismissDelay}
\description{
Specify the number of milliseconds before the tooltip (a small
lightweight window) disappears over a node or edge after appearing.
}
\usage{
setTooltipDismissDelay(obj, msecs)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{msecs}{an \code{integer}.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setTooltipInitialDelay,
  setTooltip,
  setNodeTooltipRule,
  setEdgeTooltipRule,
  setNodeTooltipDirect,
  setEdgeTooltipDirect
 }

\examples{
  \dontrun{
  # WARNING: Method RCy3::setTooltipDismissDelay() is not implemented in RCy3!
  
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'setTooltipDismissDelay demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
    # use node type as the tooltip
  setNodeTooltipRule (cw, 'type')
    # and edgeType
  setEdgeTooltipRule (cw, 'edgeType')
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
     # have the tooltips popup after 200 milliseconds, and then
     # disappear after 3000 (3 seconds)
  setTooltipInitialDelay (cw, 200)
  setTooltipDismissDelay (cw, 3000)
}

}
\keyword{graph}
