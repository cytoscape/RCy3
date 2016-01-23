\name{setEdgeLineStyleDirect}
\alias{setEdgeLineStyleDirect}
\alias{setEdgeLineStyleDirect,CytoscapeWindowClass-method}
\title{setEdgeLineStyleDirect}
\description{
In the specified CytoscapeWindow, set the line style of the specified edge
or edges, bypassing all rule mapping. The getLineStyles method shows
the possible values. 
}
\usage{
setEdgeLineStyleDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.values}{one or more \code{String} object, from the supported set
        of line styles (see getLineStyles).}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeLineStyleRule
  setEdgeColorDirect
  setEdgeFontSizeDirect
  setEdgeLabelColorDirect
  setEdgeLabelDirect
  setEdgeLabelOpacityDirect
  setEdgeLabelWidthDirect
  setEdgeLineStyleDirect
  setEdgeLineWidthDirect
  setEdgeOpacityDirect
  setEdgeSourceArrowColorDirect
  setEdgeSourceArrowDirect
  setEdgeSourceArrowOpacityDirect
  setEdgeSourceArrowShapeDirect
  setEdgeTargetArrowColorDirect
  setEdgeTargetArrowDirect
  setEdgeTargetArrowOpacityDirect
  setEdgeTargetArrowShapeDirect
  setEdgeTooltipDirect
  }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLineStyleDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  edges.of.interest <- as.character (cy2.edge.names (cw@graph))
  supported.styles <- getLineStyles (cw)

    # pass three edges and three styles
  setEdgeLineStyleDirect (cw, edges.of.interest, supported.styles [5:7])

    # pass three edges and one style
  setEdgeLineStyleDirect (cw, edges.of.interest, supported.styles [8])

    # now loop through all of the styles
  for (style in supported.styles) {
    setEdgeLineStyleDirect (cw, edges.of.interest, style)
  }

     # restore the default
  setEdgeLineStyleDirect (cw, edges.of.interest, 'SOLID')
}

\keyword{graph}
