\name{setEdgeOpacityRule}
\alias{setEdgeOpacityRule}
\alias{setEdgeOpacityRule,CytoscapeWindowClass-method}
\title{setEdgeOpacityRule}
\description{
Specify how data attributes -- for the specified named attribute -- is
mapped to edge opacity.}

\usage{
setEdgeOpacityRule(obj, edge.attribute.name, control.points, opacities, mode)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
rule is applied, determine the opacity of each edge.}
  \item{control.points}{a list of values, either numeric (for  interpolate mode) or character strings (for 'lookup' mode).}
  \item{opacities}{a list of opacity values, integers between 0 (invisible) and 255 (completely visible)}
  \item{mode}{either 'interpolate' or 'lookup'.}
}

\value{
None.
}
\author{Paul Shannon}
\seealso{
  setEdgeLineStyleRule
  setEdgeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeOpacityRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')

  edgeType.values <- c ("phosphorylates", "synthetic lethal", "undefined")

     # want to see edges and both arrows, to check success of opacity rule
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, rep ('ARROW', 3))
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, rep ('ARROW', 3))
  setDefaultEdgeLineWidth (cw, 5)

  redraw (cw)

    # do the lookup rule
  opacities <- c (25, 100, 255)
  setEdgeOpacityRule (cw, 'edgeType',  edgeType.values, opacities, mode='lookup')
  redraw (cw)

    # now do the interpolated version
  opacities <- c (10, 125, 255)
  control.points <- c (-12, 0, 35)
  setEdgeOpacityRule (cw, 'score',  control.points, opacities, mode='interpolate')  
  redraw (cw)
}

\keyword{graph}
