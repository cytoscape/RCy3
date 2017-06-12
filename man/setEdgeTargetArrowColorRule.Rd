\name{setEdgeTargetArrowColorRule}
\alias{setEdgeTargetArrowColorRule}
\alias{setEdgeTargetArrowColorRule,CytoscapeWindowClass-method}
\title{Specify rule for the target arrow color}
\description{
Specify how edge attributes -- that is, data values of the specified edge
attribute -- control the color of the target arrow, found at the end of
an edge, where it connects to the target node.}

\usage{
setEdgeTargetArrowColorRule(obj, edge.attribute.name, control.points, colors, 
                            mode="interpolate", default.color='#000000',
                            vizmap.style.name = 'default')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will determine
        the color of the target arrow of each edge when this rule is applied.}
  \item{control.points}{A list of scalar, discrete values. For instance,
        interaction types: 'phosphorylates', 'ubiquinates', 'represses', 'activates'}
  \item{colors}{A color for each of the attribute.values}
  \item{mode}{either 'interpolate' or 'lookup'.}
  \item{default.color}{The color to use when an explicit mapping is not
        provided.}
  \item{vizmap.style.name}{visual style name, defaults to the style 'default'.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  \code{\link{setNodeBorderColorRule}} (detailed example)
  \code{\link{setEdgeSourceArrowColorRule}}
  \code{\link{setEdgeColorRule}}
  \code{\link{setNodeShapeRule}}
}
\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  # send and display network
  cw <- CytoscapeWindow ('setEdgeTargetArrowColorRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  
  # add edge arrows
  arrows <- c ('CIRCLE', 'ARROW', 'DIAMOND')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)
  
  colors <- c ("#AA00AA", "#AAAA00", "#AA0000")
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  
  # set rule
  setEdgeTargetArrowColorRule (cw, 'edgeType', edgeType.values, colors, mode='lookup')
  
  # if not specified, the mode is interpolate
  colors <- c ("#FFFFFF", "#00FF00", "#00AA00", "#FF0000", "#AA0000")
  control.points <- c( -12.0, 35.0, 0.0 )
  setEdgeTargetArrowColorRule(cw, 'score', control.points, colors)
}

}
\keyword{graph}
