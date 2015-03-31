\name{fitSelectedContent}
\alias{fitSelectedContent}
\alias{fitSelectedContent,CytoscapeWindowClass-method}
\title{fitSelectedContent}
\description{
  Using all of the available window (the Cytoscape drawing canvas)
  display the current graph.
}
\usage{
fitSelectedContent(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setZoom
  fitContent
 }

\examples{
  cw <- new.CytoscapeWindow ('fitSelectedContent.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setZoom (cw, 0.1)
  selectNodes (cw, 'A')
  fitSelectedContent (cw)
  setZoom (cw, 10.0)
  fitSelectedContent (cw)
}


\keyword{graph}
