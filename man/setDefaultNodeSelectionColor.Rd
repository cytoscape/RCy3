\name{setDefaultNodeSelectionColor}
\alias{setDefaultNodeSelectionColor}
\alias{setDefaultNodeSelectionColor,CytoscapeConnectionClass-method}
\title{setDefaultNodeSelectionColor}
\description{
Retrieve the default color used to display selected nodes.
}
\usage{
setDefaultNodeSelectionColor(obj, new.color, vizmap.style.name)
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

  cw = CytoscapeWindow ("demo.setDefaultNodeSelectionColor", graph=makeSimpleGraph ())
  displayGraph (cw)
  print (setDefaultNodeSelectionColor (cw, '#FF0000'))
  redraw (cw)
}

\keyword{graph}
