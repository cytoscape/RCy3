\name{setEdgeTargetArrowColorRule}
\alias{setEdgeTargetArrowColorRule}
\alias{setEdgeTargetArrowColorRule,CytoscapeWindowClass-method}
\title{Specify Rule for the Target Arrow Color}
\description{
Specify how edge attributes -- that is, data values of the specified edge
attribute -- control the color of the target arrow, found at the end of an edge, where it connects to the
  target node.}

\usage{
setEdgeTargetArrowColorRule(obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
ColorRule is applied, determine the color of the target arrow of each edge.}
  \item{control.points}{A list of scalar, discrete values. For
  instance, interaction types:  'phosphorylates', 'ubiquinates',
  'represses', 'activates' }
\item{colors}{A color for each of the attribute.values}
\item{mode}{either 'interpolate' or 'lookup'.}
\item{default.color}{The color to use when an explicit mapping is not
  provided.)}

}

\value{
None.
}
\author{Paul Shannon}

\seealso{ \code{\link{setEdgeSourceArrowColorRule}}}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTargetArrowColorRule.test', graph=makeSimpleGraph())
  colors <- c ("#AA00AA", "#AAAA00", "#AA0000")
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeTargetArrowColorRule (cw, 'edgeType', edgeType.values, colors)
}

\keyword{graph}

