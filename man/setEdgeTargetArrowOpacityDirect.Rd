\name{setEdgeTargetArrowOpacityDirect}
\alias{setEdgeTargetArrowOpacityDirect}
\alias{setEdgeTargetArrowOpacityDirect,CytoscapeWindowClass-method}
\title{setEdgeTargetArrowOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the target arrow of the specified edge
or edges.  Opacity is an integer between 0 (invisible) and 255 (fully rendered).
}
\usage{
setEdgeTargetArrowOpacityDirect(obj, edge.names, new.values)
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

  cw <- CytoscapeWindow ('setEdgeTargetArrowOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (cw@graph))

     # make sure the target arrows are visible
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, 'Circle')

    # first try passing three edges and three arrow opacity values
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, c (64, 128, 255))
  redraw (cw)
  
    # now try passing three edges and one opacity value
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, 32)
  redraw (cw)

    # now loop through all of the arrow.opacitys

  for (opacity in seq (0, 255, by=45)) {
    setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, opacity)
    redraw (cw)
    }

    # restore the default
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, 255)
  redraw (cw)

}

\keyword{graph}
