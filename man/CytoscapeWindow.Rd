\name{CytoscapeWindow}
\alias{CytoscapeWindow}
\title{
CytoscapeWindow
}
\description{
The constructor for the CytoscapeWindowClass
}
\usage{
CytoscapeWindow(title, graph = new("graphNEL", edgemode='directed'),
                 host = "localhost", port = 1234, 
                 create.window = TRUE, overwriteWindow=FALSE, collectTimings=FALSE)
}

\arguments{
  \item{title}{
    A character string, this is the name you will see on the Cytoscape
    network window.  Multiple windows with the same name are not permitted.
}
  \item{graph}{
    A Bioconductor graph. 
}
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate CyREST plugin.
}
  \item{port}{
    Defaults to 1234, this may be any port to which CyREST
     is listening.
}
  \item{create.window}{
     Defaults to TRUE, but if you want a CytoscapeWindow just to call
     what in Java we would call 'class methods' -- getWindowList () for
     instance, a CytoscapeWindow without an actual window can be useful.
}
  \item{overwriteWindow}{
     Every Cytoscape window must have a unique title.  If the title you supply is already in use,
     this method will fail unless you specify TRUE for this parameter, in which case the
     pre-existing window with the same title will be deleted before this new one is created.
}
  \item{collectTimings}{
     Default FALSE.  Will record and report the time required to send a graph to Cytoscape.
}
}
\value{
  An object of the CytoscapeWindow Class.
}
\author{
  Paul Shannon
}

\seealso{
  CytoscapeWindow
  existing.CytoscapeWindow,
  predictTimeToDisplayGraph
  }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('new.demo', new ('graphNEL'))
}

\keyword{ graph }
