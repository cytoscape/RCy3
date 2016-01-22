\name{setDefaultNodeReverseSelectionColor}
\alias{setDefaultNodeReverseSelectionColor}
\alias{setDefaultNodeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{setDefaultNodeReverseSelectionColor}
\description{
Set the default color used to display selected nodes.
}
\usage{
setDefaultNodeReverseSelectionColor(obj, new.color, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{character} object, in quoted hexadecimal format. }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default. }
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw = CytoscapeWindow ("demo.setDefaultNodeReverseSelectionColor", graph=makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setDefaultNodeReverseSelectionColor (cw, '##FFFF00') # yellow
  # redraw (cw) --> not required anymore
}

\keyword{graph}
