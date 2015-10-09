\name{setDefaultBackgroundColor}
\alias{setDefaultBackgroundColor}
\alias{setDefaultBackgroundColor,CytoscapeConnectionClass-method}
\title{setDefaultBackgroundColor}
\description{
Retrieve the default color for the next CytoscapeWindow.
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
A character string, eg "java.awt.Color[r=204,g=204,b=255]"
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  setDefaultBackgroundColor (cy, '#CCCC00')
}

\keyword{graph}
