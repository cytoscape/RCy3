\name{saveImage}
\alias{saveImage}
\alias{saveImage,CytoscapeWindowClass-method}
\title{saveImage}
\description{
Write an image of the specified type to the specified file. For image type 'png'
there is an option to set the height of the image (see argument h). Note: the file
is written to the file system of the computer upon which R is running, not 
Cytoscape -- in those cases where they are different. It is saved to the working
directory.
}
\usage{
saveImage(obj, file.name, image.type, h = 600)
}
\arguments{

  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{file.name}{a \code{char} object. Use an explicit, full path, or
  this file will be written into your home directory.}
  \item{image.type}{a \code{char} object. 'png', 'pdf', 'svg' are the only image types currently supported}
  \item{h}{a \code{numeric} object. The height of the image. Width will be automatically set based on the height. Option only available for 'png' image type. All other image types use the default of 600 for the height and it is not adjustable.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

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
  filename <- paste (getwd (), 'saveImageTest', sep='/')
  saveImage (cw, filename, 'svg')   # currently supports svg, pdf and png
  saveImage(cw, filename, 'png', 1600)
}

\keyword{graph}
