\name{setEdgeColorRule}
\alias{setEdgeColorRule}
\alias{setEdgeColorRule,CytoscapeWindowClass-method}
\title{setEdgeColorRule}
\description{
Specify how data attributes -- for the specified named attribute -- is
mapped to edge color.}

\usage{
setEdgeColorRule(obj, edge.attribute.name, control.points, colors, mode, default.color='#FFFFFF')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
rule is applied, determine the color of each edge.}
  \item{control.points}{a list of values, either numeric (for  interpolate mode) or character strings (for 'lookup' mode).}
  \item{colors}{a list of colors, expressed as hexadecimal RGB, like this:  '#FF0000' or '#FA8800'}
  \item{mode}{either 'interpolate' or 'lookup'.}
  \item{default.color}{a \code{String} object, expressed in 
hexadecimal RGB, like this:  '#FF0000' or '#FA8800'}
}

\value{
None.
}
\author{Paul Shannon}
\seealso{
  setEdgeLineStyleRule
  setNodeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeColorRule.test', graph=makeSimpleGraph())
  displayGraph(cw)
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  colors = c ('#FF0000', '#FFFF00', '#00FF00')
  setEdgeColorRule (cw, 'edgeType',  edgeType.values, colors, mode='lookup')

  score.values = c (-15, 0, 40);
  colors = c ('#00FF00', '#FFFFFF', '#FF0000')
  setEdgeColorRule (cw, 'score',  score.values, colors, mode='interpolate')
    # now swap the colors around
    # now swap the colors
  colors = c ('#FF0000', '#FFFFFF', '#00FF00')
  setEdgeColorRule (cw, 'score',  score.values, colors, mode='interpolate')

  redraw (cw)
}

\keyword{graph}
