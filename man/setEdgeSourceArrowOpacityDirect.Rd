\name{setEdgeSourceArrowOpacityDirect}
\alias{setEdgeSourceArrowOpacityDirect}
\alias{setEdgeSourceArrowOpacityDirect,CytoscapeWindowClass-method}
\title{setEdgeSourceArrowOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the source arrow of the specified edge
or edges.  Opacity is an integer between 0 (invisible) and 255 (fully rendered).
}
\usage{
setEdgeSourceArrowOpacityDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more cy2-style edge names, \code{String} objects.}
  \item{new.values}{one or more \code{integer} objects, between 0 and 255.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  cy2.edge.names
  setEdgeTargetArrowOpacityDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeSourceArrowOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (cw@graph))
     # make sure the source arrows are visible
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'Circle')

    # first try passing three edges and three arrow opacity values
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, c (64, 128, 255))
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one opacity value
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, 32)
  redraw (cw)

    # now loop through all of the arrow.opacitys

  for (opacity in seq (0, 255, by=45)) {
    setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, opacity)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, 255)
  redraw (cw)

}

\keyword{graph}
