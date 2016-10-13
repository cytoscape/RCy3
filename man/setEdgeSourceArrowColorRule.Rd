\name{setEdgeSourceArrowColorRule}
\alias{setEdgeSourceArrowColorRule}
\alias{setEdgeSourceArrowColorRule,CytoscapeWindowClass-method}
\title{Specify Rule for the Source Arrow Color}
\description{
Specify how edge attributes -- that is, data values of the specified edge
attribute -- control the color of the source arrow, found at the end of
an edge, where it connects to the source node.}

\usage{
setEdgeSourceArrowColorRule(obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will determine
        the color of the source arrow for each edge when this ColorRule is applied.}
  \item{control.points}{A list of scalar, discrete values. For instance,
        interaction types: 'phosphorylates', 'ubiquinates', 'represses', 'activates'}
\item{colors}{A color for each of the attribute.values}
\item{mode}{either 'interpolate' or 'lookup'.}
\item{default.color}{The color to use when an explicit mapping is not provided.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{ \code{\link{setEdgeTargetArrowColorRule}}}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  # send and display graph
  cw <- CytoscapeWindow ('setEdgeSourceArrowColorRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw, 'force-directed')
  
  colors <- c ("#AA00AA", "#AAAA00", "#AA0000")
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  
  # add edge arrows
  arrows <- c ('Arrow', 'Diamond', 'Circle')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, arrows)
  
  # set rule
  setEdgeSourceArrowColorRule (cw, 'edgeType', edgeType.values, colors, mode='lookup')
  
  # if not specified, the mode is interpolate
  colors <- c ("#FFFFFF", "#00FF00", "#00AA00", "#FF0000", "#AA0000")
  control.points <- c( -12.0, 35.0, 0.0 )
  setEdgeSourceArrowColorRule (cw, 'score', control.points, colors)
}

}
\keyword{graph}
