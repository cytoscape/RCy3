\name{apiVersion}
\alias{apiVersion}
\alias{apiVersion,CytoscapeConnectionClass-method}
\title{apiVersion}
\description{
Get the currently used plugin version number of CyREST.
}
\usage{
apiVersion(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
  a string describing the current version of the CyREST app.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (apiVersion (cy))
    # e.g. "v1"
}

}
\keyword{graph}
