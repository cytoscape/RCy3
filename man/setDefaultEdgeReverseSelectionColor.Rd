\name{setDefaultEdgeReverseSelectionColor}
\alias{setDefaultEdgeReverseSelectionColor}
\alias{setDefaultEdgeReverseSelectionColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeReverseSelectionColor}
\description{
Retrieve the default color used to display selected edges.
}
\usage{
setDefaultEdgeReverseSelectionColor(obj, new.color, vizmap.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{character} object, in quoted hexadecimal format }
  \item{vizmap.style.name}{a \code{character} object, 'default' by default }

}

\value{
Nothing.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw = CytoscapeWindow ("demo.setDefaultEdgeReverseSelectionColor", graph=makeSimpleGraph ())
  displayGraph (cw)
  print (setDefaultEdgeReverseSelectionColor (cw, '#FF0000'))
  redraw (cw)
}

\keyword{graph}
