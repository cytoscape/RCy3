\name{unhideAll}
\alias{unhideAll}
\alias{unhideAll,CytoscapeWindowClass-method}
\title{unhideAll}
\description{
Currently (in Cytoscape 2.7) broken.  The redisplay of hidden nodes and
edges does not alwasy work...
}
\usage{
unhideAll(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
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
  cw <- new.CytoscapeWindow ('unhideAll.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
  clearSelection (cw)
  selectNodes (cw, 'A')
  hideSelectedNodes (cw)
  system ('sleep 2')
  unhideAll (cw)
}


\keyword{graph}
