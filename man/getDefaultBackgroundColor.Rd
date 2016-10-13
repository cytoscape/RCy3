\name{getDefaultBackgroundColor}
\alias{getDefaultBackgroundColor}
\alias{getDefaultBackgroundColor,CytoscapeConnectionClass-method}
\title{getDefaultBackgroundColor}
\description{
Retrieve the default background color for the next CytoscapeWindow.
}
\usage{
getDefaultBackgroundColor(obj, vizmap.style.name)
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
  print (getDefaultBackgroundColor (cy)) # a hex number
    # [1] "#FFFFFF"
}

}
\keyword{graph}
