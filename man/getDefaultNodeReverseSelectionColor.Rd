\name{getDefaultNodeReverseSelectionColor}
\alias{getDefaultNodeReverseSelectionColor}
\alias{getDefaultNodeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{getDefaultNodeReverseSelectionColor}
\description{
Retrieve the default color used to display not selected nodes.
}
\usage{
getDefaultNodeReverseSelectionColor(obj, vizmap.style.name)
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
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  print (getDefaultNodeReverseSelectionColor (cy))
    # [1] "#1E90FF"
}

}
\keyword{graph}
