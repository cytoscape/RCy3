\name{deleteEdgeAttribute}

\alias{deleteEdgeAttribute}
\alias{deleteEdgeAttribute,CytoscapeConnectionClass-method}

\title{deleteEdgeAttribute}
\description{
Node and edge attributes are usually added to a Cytoscape network by
defining them on the graph used to construct a CytoscapeWindow.
Once Cytoscape has been passed an attribute, however, it persists
until you exit the application or delete it by using the Cytoscape
graphical user interface or by calling this method.
}
\usage{
deleteEdgeAttribute(obj, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
  \item{attribute.name}{a \code{character string}, the name of the
    attribute you wish to delete.}
}

\value{
nothing
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getEdgeAttributeNames
  addEdgeAttribute
  deleteNodeAttribute
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.deleteEdgeAttribute'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  setDefaultEdgeLineWidth (cw, 5);
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)
  
    # before:
  print (getEdgeAttributeNames (cw))
    # [1] "name"        "interaction" "edgeType"    "score"       "misc"  
  deleteEdgeAttribute (cw, 'score')
    # after
  print (getEdgeAttributeNames (cw))
    # [1] "name"        "interaction" "edgeType"    "misc" 
}

}
\keyword{graph}
