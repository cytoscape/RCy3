\name{getNodeAttributeNames}

\alias{getNodeAttributeNames}
\alias{getNodeAttributeNames,CytoscapeConnectionClass-method}

\title{getNodeAttributeNames}
\description{
Node and node attributes belong to the Cytoscape session as a whole, not
to a particular window.  Use this method to find out the names of the
currently defined node attributes.
}
\usage{
getNodeAttributeNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
}

\value{
A list of names.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getNodeAttribute
  deleteNodeAttribute
  getEdgeAttributeNames
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('demo.getNodeAttributeNames', graph=makeSimpleGraph())
  displayGraph (cw)
  # displayGraph is required. If you do not display the graph,
  # your R graph data has not been sent to Cytoscape yet.
  print (getNodeAttributeNames (cw))
    # [1] "name"  "type"  "lfc"   "label" "count"
}

}
\keyword{graph}
