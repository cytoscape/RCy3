\name{getSelectedEdges}
\alias{getSelectedEdges}
\alias{getSelectedEdges,CytoscapeWindowClass-method}
\title{getSelectedEdges}
\description{
Retrieve the identifiers of all the edges selected in the current graph.
}
\usage{
getSelectedEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getSelectedEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
    # in Cytoscape, interactively select an edge or call the selectEdges function
  selectEdges (cw, "A (phosphorylates) B")
  getSelectedEdges (cw)
    # [1] "A (phosphorylates) B"
}

}
\keyword{graph}
