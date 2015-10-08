\name{ping}
\alias{ping}
\alias{ping,CytoscapeConnectionClass-method}
\title{ping}
\description{
Test the connection to Cytoscape.  
}
\usage{
ping(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
  "It works!"
}

\author{Paul Shannon}

\examples{
  cy <- CytoscapeConnection ()
  ping (cy)
    # "It works!"   
}

\keyword{graph}
