\name{createWindowFromSelection}
\alias{createWindowFromSelection}
\alias{createWindowFromSelection,CytoscapeWindowClass-method}
\title{createWindowFromSelection}
\description{
  All selected nodes, their connecting edges, and associated attributes are
  copied into a new CytoscapeWindow, with the supplied title.
}
\usage{
createWindowFromSelection(obj, new.windowTitle, return.graph)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.windowTitle}{a \code{String}.}
  \item{return.graph}{an \code{logical} object.}
}

\value{
A new CytoscapeWindow object, with the graph slot populated with the new
selected subgraph, if requested. If not requested, the graph slot holds
an empty graph.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
 selectNodes 
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  title <- 'createWindowFromSelection demo'

  cw <- CytoscapeWindow (title, makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw)
  selectNodes (cw, c ('A', 'C'))
 
  new.window.title <- 'NEW WINDOW'
  c2 <- createWindowFromSelection (cw, new.window.title, TRUE)
  layoutNetwork (c2, 'force-directed')
  print (getEdgeCount(c2))  # should be 1
}

}
\keyword{graph}
