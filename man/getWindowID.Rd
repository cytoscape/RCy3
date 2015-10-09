\name{getWindowID}
\alias{getWindowID}
\alias{getWindowID,CytoscapeConnectionClass-method}
\title{getWindowID}
\description{
Windows in Cytoscape have both a title and an identifier.  The title is
useful for human readers; the identifier is used by Cytoscape internals,
and is sometimes useful to obtain.  This method returns the identifier
associated with the window title.
}
\usage{
getWindowID(obj, window.title)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{window.title}{a \code{string}. }
}

\value{
The identifier (id) of a window, which is always a string -- even if the
identifer appears to be an integer.
}
\author{Paul Shannon}

\seealso{
  getWindowList
  }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  cw <- CytoscapeWindow ('getWindowID.test', graph=makeSimpleGraph())
  displayGraph (cw)
  getWindowID (cy, 'getWindowID.test')
}

\keyword{graph}
