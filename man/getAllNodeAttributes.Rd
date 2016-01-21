\name{getAllNodeAttributes}

\alias{getAllNodeAttributes}
\alias{getAllNodeAttributes,CytoscapeWindowClass-method}

\title{getAllNodeAttributes}
\description{
Create a data frame with all the node attributes for the graph contained
by the supplied CytoscapeWindow object.  Only the local copy of the
graph is queried. If you want all the (possibly different) node attributes from the
Cytoscape network which corresponds to this graph, one option is to
create a new CytoscapeWindow; see the existing.CytoscapeWindow function.

}
\usage{
getAllNodeAttributes(obj, onlySelectedNodes=FALSE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object
    object. }
  \item{onlySelectedNodes}{a \code{logical} variable, used to restrict
    the query.}
}

\value{
A data frame, with a column for each attribute, a row for each node.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getNodeAttribute
  deleteNodeAttribute
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.getAllNodeAttributes'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)
    # get all attributes for all nodes
  tbl.noa = getAllNodeAttributes (cw, onlySelectedNodes=FALSE)
  tbl.noa
  #                   type lfc  label count
  # A               kinase  -3 Gene A     2
  # B transcription factor   0 Gene B    30
  # C         glycoprotein   3 Gene C   100
  
  selectNodes(cw, 'A')
  getAllNodeAttributes (cw, TRUE)
  #     type lfc  label count
  # A kinase  -3 Gene A     2
}

\keyword{graph}
