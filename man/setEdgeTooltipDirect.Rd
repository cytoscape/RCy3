\name{setEdgeTooltipDirect}
\alias{setEdgeTooltipDirect}
\alias{setEdgeTooltipDirect,CytoscapeWindowClass-method}
\title{setEdgeTooltipDirect}
\description{
In the specified CytoscapeWindow, set the tooltips of the specified edge
or edges.  The tooltips are not available until redraw is called.
}
\usage{
setEdgeTooltipDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more cy2-style edge names, \code{String} objects.}
  \item{new.values}{one or more \code{String} objects.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  cy2.edge.names
  setEdgeTooltipRule
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTooltipDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (cw@graph))

    # first try passing three edges and three tooltips
  setEdgeTooltipDirect (cw, edges.of.interest, c ('tooltip #1', 'tooltip #2', 'tooltip #3'))
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one tooltip
  setEdgeTooltipDirect (cw, edges.of.interest, 'a general purpose tooltip')
  redraw (cw)

  setEdgeTooltipDirect (cw, edges.of.interest, '')
  redraw (cw)

}

\keyword{graph}
