\name{pluginVersion}
\alias{pluginVersion}
\alias{pluginVersion,CytoscapeConnectionClass-method}
\title{pluginVersion}
\description{
Get the currently used plugin version number of CyREST.
}
\usage{
pluginVersion(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
  a string describing the current version of the CyREST plugin.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (pluginVersion (cy))
    # e.g. "v1"
}

\keyword{graph}
