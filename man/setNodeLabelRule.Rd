\name{setNodeLabelRule}
\alias{setNodeLabelRule}
\alias{setNodeLabelRule,CytoscapeWindowClass-method}
\title{setNodeLabelRule}
\description{
Specify the node attribute to be used as the label for each node.
Non-character attributes are converted to strings before they are
used as labels.}

\usage{
setNodeLabelRule(obj, node.attribute.name)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will
        determine the label on each node when this rule is applied.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeLabelRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeLabelRule (cw, 'label')
  setNodeLabelRule (cw, 'type')
  setNodeLabelRule (cw, 'lfc')
  setNodeLabelRule (cw, 'count')
}

\keyword{graph}
