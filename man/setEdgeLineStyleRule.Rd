\name{setEdgeLineStyleRule}
\alias{setEdgeLineStyleRule}
\alias{setEdgeLineStyleRule,CytoscapeWindowClass-method}
\title{specify the line styles to be used in drawing edges}
\description{
Specify how data attributes -- for the specified named attribute -- are
mapped to edge line style.}

\usage{
setEdgeLineStyleRule(obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
rule is applied, determine the lineStyle of each edge.}
  \item{attribute.values}{A list of scalar, discrete values. For
  instance, interaction types:  'phosphorylates', 'ubiquinates',
  'represses', 'activates' }
\item{line.styles}{One line style for each of the attribute.values}
\item{default.style}{The style to use when an explicit mapping is not provided.}

}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  \code{\link{getLineStyles}}
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLineStyleRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  line.styles     <- c ('SINEWAVE',       'DOT',              'PARALLEL_LINES')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeLineStyleRule (cw, 'edgeType', edgeType.values, line.styles)
  redraw (cw)
}

\keyword{graph}


