\name{setNodeImageDirect}
\alias{setNodeImageDirect}
\alias{setNodeImageDirect,CytoscapeWindowClass-method}
\title{setNodeImageDirect}
\description{
In the specified CytoscapeWindow, set the images of the specified nodes.
}
\usage{
setNodeImageDirect(obj, node.names, image.urls)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{image.urls}{one or more \code{String} objects. If just one, then
    this is replicated for each of the supplied node.names.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeShapeDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeImageDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeImageDirect (cw, 'C', 'http://rcytoscape.systemsbiology.net/versions/current/images/bioc.png')
  redraw (cw)
}


\keyword{graph}
