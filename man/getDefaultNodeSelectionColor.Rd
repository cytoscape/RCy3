\name{getDefaultNodeSelectionColor}
\alias{getDefaultNodeSelectionColor}
\alias{getDefaultNodeSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultNodeSelectionColor}
\description{
Retrieve the default color used to display selected nodes.
}
\usage{
getDefaultNodeSelectionColor(obj, vizmap.style.name)
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
  print (getDefaultNodeSelectionColor (cy))
    # [1] "#FFFF00"
}

}
\keyword{graph}
