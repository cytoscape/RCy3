\name{saveImage}
\alias{saveImage}
\alias{saveImage,CytoscapeWindowClass-method}
\title{saveImage}
\description{
Write an image of the specified type to the specified file, at the
specified scaling factor.  Note:  the file is written to the file system
of the computer upon which Cytoscape is running, not R -- in those cases
where they are different.
}
\usage{
saveImage(obj, file.name, image.type, scale)
}
\arguments{

  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{file.name}{a \code{char} object. Use an explicit, full path, or
  this file will be written into your home directory.}
  \item{image.type}{a \code{char} object. 'jpg' is the only image type   currently supported}
  \item{scale}{a \code{numeric} object.  How large (or small) to  scale the image. }

}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectNodes
  clearSelection
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('saveImage.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  filename = tempfile () # paste (getwd (), 'saveImageTest.jpg', sep='/')
  # saveImage (cw, filename, 'jpg', 2.0)   # doesn't yet work reliably at bioc
}


\keyword{graph}
