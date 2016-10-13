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
  \item{edge.attribute.name}{the edge attribute whose values will
        determine the tooltip on each edge when this rule is applied.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeTooltipDirect
 }
 
\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTooltipRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setEdgeTooltipRule (cw, 'edgeType')
}

}
\keyword{graph}
