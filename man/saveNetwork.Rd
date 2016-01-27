\name{saveNetwork}
\alias{saveNetwork}
\alias{saveNetwork,CytoscapeWindowClass-method}
\title{saveNetwork}
\description{
Write a network of the specified type to the specified file, at the
specified scaling factor.  Note:the file is written to the file system
of the computer upon which R is running, not Cytoscape -- in those cases
where they are different. It is saved to the working directory.
}
\usage{
saveNetwork(obj, file.name, format='cys')
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{file.name}{a \code{char} object. }
  \item{format}{a \code{char} object. 'cys' is the only type currently supported}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  saveImage
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('saveNetwork.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  redraw (cw)
  filename <- paste (getwd (), 'saveNetworkTest', sep='/')
  saveNetwork (cw, filename)
  print (sprintf ('cys file exists? %s', file.exists (paste0(filename, '.cys'))))
}

\keyword{graph}
