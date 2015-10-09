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
selected subgraph, if requested.  If not requested, the graph slot holds
an empty graph.
}
\author{Paul Shannon}

\seealso{
 selectNodes 
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  title <- 'createWindowFromSelection demo'

  cw <- CytoscapeWindow (title, makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)
  selectNodes (cw, c ('A', 'C'))
 
  new.window.title <- 'NEW WINDOW'
  if (new.window.title \%in\% as.character (getWindowList (cy)))
    deleteWindow (cy, new.window.title)

  #c2 <- createWindowFromSelection (cw, new.window.title, TRUE)
  #redraw (c2)
  #layoutNetwork(c2)

  #clearSelection (c2)
  #selectNodes (c2, 'C')
  #print (getSelectedNodeCount (c2))  # should be 1
}
\keyword{graph}
