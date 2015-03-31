\name{setEdgeTooltipRule}
\alias{setEdgeTooltipRule}
\alias{setEdgeTooltipRule,CytoscapeWindowClass-method}
\title{setEdgeTooltipRule}
\description{
Specify the edge attribute to be used as the tooltip for each edge.
Non-character attributes are converted to strings before they are used
as tooltips.}

\usage{
setEdgeTooltipRule(obj, edge.attribute.name)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.attribute.name}{the edge attribute whose values will, when this
        rule is applied, determine the tooltip on each edge.}
}

\value{
None.
}
\author{Paul Shannon}

\examples{
  cw <- new.CytoscapeWindow ('setEdgeTooltipRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
  setEdgeTooltipRule (cw, 'edgeType')
}

\keyword{graph}
