\name{getDefaultEdgeReverseSelectionColor}
\alias{getDefaultEdgeReverseSelectionColor}
\alias{getDefaultEdgeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultEdgeReverseSelectionColor}
\description{
Retrieve the default color used to display not selected edges.
}
\usage{
getDefaultEdgeReverseSelectionColor(obj, vizmap.style.name)
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
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (getDefaultEdgeReverseSelectionColor (cy))
    # [1] "#323232"
}

}
\keyword{graph}
