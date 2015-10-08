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
  cy <- CytoscapeConnection ()
  print (pluginVersion (cy))
    # e.g., "1.7"
}

\keyword{graph}
