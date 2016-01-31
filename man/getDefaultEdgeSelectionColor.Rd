\name{getDefaultEdgeSelectionColor}
\alias{getDefaultEdgeSelectionColor}
\alias{getDefaultEdgeSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultEdgeSelectionColor}
\description{
Retrieve the default color used to display selected edges.
}
\usage{
getDefaultEdgeSelectionColor(obj, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default }
}

\value{
A character string, a hexadecimal, e.g. #000000
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (getDefaultEdgeSelectionColor (cy))
    # [1] "#FF0000"
}

\keyword{graph}
