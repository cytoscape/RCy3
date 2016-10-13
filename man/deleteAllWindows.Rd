\name{deleteAllWindows}
\alias{deleteAllWindows}
\alias{deleteAllWindows,CytoscapeConnectionClass-method}
\title{deleteAllWindows}
\description{
Delete all the network windows currently held by Cytoscape, removing
them from the screen, and deleting Cytoscape's copy of all of the
graphs.  The R graphs are unchanged.
}
\usage{
deleteAllWindows(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  dontrun{
  cy <- CytoscapeConnection ()
  cw1 = CytoscapeWindow ('cw1')
  cw2 = CytoscapeWindow ('cw2')
  deleteAllWindows (cy)
}

}
\keyword{graph}
