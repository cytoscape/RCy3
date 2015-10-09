\name{pluginVersion}
\alias{pluginVersion}
\alias{pluginVersion,CytoscapeConnectionClass-method}
\title{pluginVersion}
\description{
Test the connection to Cytoscape.  
}
\usage{
pluginVersion(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
  "A string describing the current version of the CytoscapeRPC plugin."
}

\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (pluginVersion (cy))
    # e.g., "1.7"
}

\keyword{graph}
