\name{getDefaultBackgroundColor}
\alias{getDefaultBackgroundColor}
\alias{getDefaultBackgroundColor,CytoscapeConnectionClass-method}
\title{getDefaultBackgroundColor}
\description{
Retrieve the default color for the next CytoscapeWindow.
}
\usage{
getDefaultBackgroundColor(obj, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default }
}

\value{
A character string, eg "java.awt.Color[r=204,g=204,b=255]"
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (getDefaultBackgroundColor (cy))   # "java.awt.Color[r=204,g=204,b=255]"
}

\keyword{graph}
