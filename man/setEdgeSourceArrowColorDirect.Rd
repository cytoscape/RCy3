\name{setEdgeSourceArrowColorDirect}
\alias{setEdgeSourceArrowColorDirect}
\alias{setEdgeSourceArrowColorDirect,CytoscapeWindowClass-method}
\title{setEdgeSourceArrowColorDirect}
\description{
In the specified CytoscapeWindow, set the edgeSourceArrowColor
of the specified edge or edges.
}
\usage{
setEdgeSourceArrowColorDirect(obj, edge.names, new.colors)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, edges in standard
        Cytoscape form.}
  \item{new.colors}{one or more \code{String} objects, representing a color in a
    '#RRGGBB' hex format.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeTargetArrowColorDirect
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeSourceArrowColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw, 'force-directed')

  arrows <- c ('Arrow', 'Diamond', 'Circle')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, arrows)
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)

  colors.1 <- c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 <- c ("#AA00AA", "#00AAAA", "#0000AA")

  edge.names <- as.character (cy2.edge.names (cw@graph)) [1:3]

  for (i in 1:2) {
    setEdgeSourceArrowColorDirect (cw, edge.names, colors.1)
    Sys.sleep (0.3)
    setEdgeSourceArrowColorDirect (cw, edge.names, colors.2)
    Sys.sleep (0.3)
    } # for i
}

}
\keyword{graph}
