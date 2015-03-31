\name{setDefaultEdgeSelectionColor}
\alias{setDefaultEdgeSelectionColor}
\alias{setDefaultEdgeSelectionColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeSelectionColor}
\description{
Retrieve the default color used to display selected edges.
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
\author{Paul Shannon}

\examples{
  cy <- CytoscapeConnection ()
  print (setDefaultEdgeSelectionColor (cy, '#FF0000'))
}

\keyword{graph}
