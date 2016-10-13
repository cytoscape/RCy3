\name{setNodeImageDirect}
\alias{setNodeImageDirect}
\alias{setNodeImageDirect,CytoscapeWindowClass-method}
\title{setNodeImageDirect}
\description{
In the specified CytoscapeWindow, set the images of the specified nodes.
}
\usage{
setNodeImageDirect(obj, node.names, image.positions)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{image.positions}{one or more \code{int} objects, specifying the
    position of the image in the Image Manager. If just one, then
    this is replicated for each of the supplied node.names.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeShapeDirect
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeImageDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  # Before running the next function, upload your image to the Image Manager
  # and determine the position of the image in the list (start counting from the bottom)
  # Images are added at the top of the interface but at the end of the list
  # There are currently 24 default images in the Image Manager
  setNodeImageDirect (cw, 'C', 1 ) # last image in the Image Manager
}

}
\keyword{graph}
