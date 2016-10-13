\name{setEdgeTooltipDirect}
\alias{setEdgeTooltipDirect}
\alias{setEdgeTooltipDirect,CytoscapeWindowClass-method}
\title{setEdgeTooltipDirect}
\description{
In the specified CytoscapeWindow, set the tooltips of the specified edge
or edges.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeTooltipRule
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTooltipDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')

  edges.of.interest <- as.character (cy2.edge.names (cw@graph))

    # first try passing three edges and three tooltips
  setEdgeTooltipDirect (cw, edges.of.interest, c ('tooltip #1', 'tooltip #2', 'tooltip #3'))

    # now try passing three edges and one tooltip
  setEdgeTooltipDirect (cw, edges.of.interest, 'a general purpose tooltip')

  setEdgeTooltipDirect (cw, edges.of.interest, '')
}

}
\keyword{graph}
