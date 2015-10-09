\name{setNodeAttributesDirect}
\alias{setNodeAttributesDirect}
\alias{setNodeAttributesDirect,CytoscapeWindowClass-method}
\title{setNodeAttributesDirect}
\description{
Transfer the named node attribute, for all named nodes, to Cytoscape.
The attribute must be previously defined on the nodes of the graph:  see
nodeDataDefaults in the graph class.  This method is useful if you wish to run a 'movie.'  For example, if
you have a timecourse experiment, with different values at successive
time points  of the 'lfc' (log fold change) measurements or 'pValue' of
each node.  With a nodeColor and nodeSize rule already specified, you can animate the
display of the nodes across time in the graph by pumping new values of the attributes
attributes using this method, and then asking for a redraw.  An example of such
node-attribute-driven animation can be found here....[todo].
}

\usage{
setNodeAttributesDirect(obj, attribute.name, attribute.type, node.names, values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string} one of the attributes defined on
    the nodes. }
  \item{attribute.type}{a \code{string} from one of these three groups:
    (floating, numeric, double), (integer, int), (string,
    char, character).  This parameter is required because RCytoscape
    cannot always infer the type of an attribute.}
  \item{node.names}{a \code{list} of strings, node names}
  \item{values}{a list of objects of the type specified by
    'attribute.name', one per node}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeAttributes
  setEdgeAttributes
  setEdgeAttributesDirect
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeAttributesDirect.test', graph=makeSimpleGraph())
  sendNodes(cw)
  result = setNodeAttributesDirect (cw, 'count', 'int', c ('A', 'B', 'C'), c (4, 8, 12))
}

\keyword{graph}
