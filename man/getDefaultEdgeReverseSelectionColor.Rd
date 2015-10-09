\name{getDefaultEdgeReverseSelectionColor}
\alias{getDefaultEdgeReverseSelectionColor}
\alias{getDefaultEdgeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultEdgeReverseSelectionColor}
\description{
Retrieve the default color used to display selected edges.
}
\usage{
getDefaultEdgeReverseSelectionColor(obj, vizmap.style.name)
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
  print (getDefaultEdgeReverseSelectionColor (cy))   # "java.awt.Color[r=0,g=255,b=0]"
}

\keyword{graph}
