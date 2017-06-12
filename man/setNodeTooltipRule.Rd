\name{setNodeTooltipRule}
\alias{setNodeTooltipRule}
\alias{setNodeTooltipRule,CytoscapeWindowClass-method}
\title{setNodeTooltipRule}
\description{
Specify the node attribute to be used as the tooltip for each node.
Non-character attributes are converted to strings before they are used
as tooltips.}

\usage{
setNodeTooltipRule(obj, node.attribute.name, vizmap.style.name = 'default')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will
        determine the tooltip on each node when this rule is applied.}
  \item{vizmap.style.name}{visual style name, defaults to the style 'default'.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeTooltipRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeTooltipRule (cw, 'type')
  setNodeTooltipRule (cw, 'lfc')
  setNodeTooltipRule (cw, 'count')
}

}
\keyword{graph}
