\name{getDefaultNodeReverseSelectionColor}
\alias{getDefaultNodeReverseSelectionColor}
\alias{getDefaultNodeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultNodeReverseSelectionColor}
\description{
Retrieve the default color used to display selected nodes.
}
\usage{
getDefaultNodeReverseSelectionColor(obj, vizmap.style.name)
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
  print (getDefaultNodeReverseSelectionColor (cy))   # "java.awt.Color[r=0,g=255,b=0]"
}

\keyword{graph}
