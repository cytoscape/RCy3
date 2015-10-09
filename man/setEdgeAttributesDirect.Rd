\name{setEdgeAttributesDirect}
\alias{setEdgeAttributesDirect}
\alias{setEdgeAttributesDirect,CytoscapeWindowClass-method}
\title{setEdgeAttributesDirect}
\description{
Transfer the named edge attribute to Cytoscape.  This method is
required, for instance, if you wish to run a 'movie.'  For example, if
you have a timecourse experiment, with different values at successive
time points  of the 'phosphorylates' or 'binds' relationship between two
nodes.   With an edgeColor rule already specified, you can animate the
display of the edges in the graph by pumping new values of the edge
attributes, and then asking for a redraw.  An example of such
edge-attribute-driven animation can be found here....[todo].
}
\usage{
setEdgeAttributesDirect(obj, attribute.name, attribute.type, edge.names, values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string} one of the attributes defined on
    the edges. }
  \item{attribute.type}{a \code{string} from one of these three groups:
     (floating, numeric, double), (integer, int), (string,
    char, character).  This parameter is required because RCytoscape
    cannot always infer the type of an attribute.}
  \item{edge.names}{a \code{list} of strings, edge names}
  \item{values}{a list of objects of the type specified by
    'attribute.name', one per edge}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setEdgeAttributes
  setNodeAttributes
  setNodeAttributesDirect
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeAttributesDirect.test', graph=makeSimpleGraph())
  sendNodes(cw)
  sendEdges(cw)
  edge.names = as.character (cy2.edge.names (cw@graph))
  stopifnot (length (edge.names) == 3)
  edge.values = c ('alligator', 'hedgehog', 'anteater')
  result = setEdgeAttributesDirect (cw, 'misc', 'string', edge.names, edge.values)
}

\keyword{graph}
