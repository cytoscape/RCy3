\name{clearSelection}
\alias{clearSelection}
\alias{clearSelection,CytoscapeWindowClass-method}
\title{clearSelection}
\description{
If any nodes are selected in the current Cytocape window, they will be unselected.
}
\usage{
clearSelection(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}


\value{
Nothing
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())
  cw<- CytoscapeWindow ('clearSelection.test', graph=makeSimpleGraph())
  displayGraph (cw)
  selectNodes (cw, 'A')
  print (getSelectedNodeCount (cw))   # should be 1
  clearSelection (cw)
  print (getSelectedNodeCount (cw))   # should be 0

}

\keyword{graph}
