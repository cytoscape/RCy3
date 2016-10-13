\name{setDefaultBackgroundColor}
\alias{setDefaultBackgroundColor}
\alias{setDefaultBackgroundColor,CytoscapeConnectionClass-method}
\title{setDefaultBackgroundColor}
\description{
Set the default color for the next CytoscapeWindow.
}
\usage{
setDefaultBackgroundColor(obj, new.color, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{character} object, in quoted hexadecimal format }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default }
}

\value{
A character string, a hexadecimal, e.g. #000000
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultBackgroundColor.test', graph=makeSimpleGraph())
  setDefaultBackgroundColor (cw, '#CCCC00')
  cw2 <- CytoscapeWindow ('setDefaultBackgroundColor.test2', graph=makeSimpleGraph())
}

}
\keyword{graph}
