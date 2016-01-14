\name{showGraphicsDetails}
\alias{showGraphicsDetails}
\alias{showGraphicsDetails,CytoscapeConnectionClass-method}
\title{showGraphicsDetails}
\description{
For all windows, and regardless of the current zoom
level, display or hide graphics details --  of which node labels are the
most obvious example.
}
\usage{
showGraphicsDetails(obj, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  showGraphicsDetails (cy)

}

\keyword{graph}
