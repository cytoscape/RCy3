\name{saveNetwork}
\alias{saveNetwork}
\alias{saveNetwork,CytoscapeWindowClass-method}
\title{saveNetwork}
\description{
Write a network of the specified type to the specified file, at the
specified scaling factor.  Note:  the file is written to the file system
of the computer upon which Cytoscape is running, not R -- in those cases
where they are different.
}
\usage{
saveNetwork(obj, file.name, format='gml')
}
\arguments{

  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{file.name}{a \code{char} object. }
  \item{format}{a \code{char} object. 'gml' is the only type  currently supported}

}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  saveImage
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('saveNetwork.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)

  #filename <- sprintf ('%s/%s', tempdir (), 'saveNetworkTest.gml')
  #not sure if this will work at bioc   
  #saveNetwork (cw, filename)
  #print (sprintf ('gml file exists? %s', file.exists (filename)))

}


\keyword{graph}
