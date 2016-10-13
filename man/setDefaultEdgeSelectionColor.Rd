\name{setDefaultEdgeSelectionColor}
\alias{setDefaultEdgeSelectionColor}
\alias{setDefaultEdgeSelectionColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeSelectionColor}
\description{
Set the default color used to display selected edges.
}
\usage{
setDefaultEdgeSelectionColor(obj, new.color, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{character} object, in quoted hexadecimal format }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default }
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw = CytoscapeWindow ("demo.setDefaultEdgeSelectionColor", graph=makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw)
  setDefaultEdgeSelectionColor (cw, '#00FF00')
  # redraw (cw) --> not required anymore
}

}
\keyword{graph}
