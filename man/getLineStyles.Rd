\name{getLineStyles}
\alias{getLineStyles}
\alias{getLineStyles,CytoscapeConnectionClass-method}
\title{getLineStyles}
\description{
Retrieve the names of the currently supported line types -- values which
can be used to render edges, and thus can be used in calls to 'setEdgeLineStyleRule'
}
\usage{
getLineStyles(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g., 'SOLID', 'DOT'
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getLineStyles (cy)
  #  [1] "SOLID" "LONG_DASH" "EQUAL_DASH" ...
}

\keyword{graph}
