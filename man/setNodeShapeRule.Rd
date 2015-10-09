\name{setNodeShapeRule}
\alias{setNodeShapeRule}
\alias{setNodeShapeRule,CytoscapeWindowClass-method}
\title{setNodeShapeRule}
\description{
Specify how data attributes how the specified node attribute values
determine the node shape.}
\usage{
setNodeShapeRule (obj, node.attribute.name=, attribute.values,
node.shapes, default.shape)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node  attribute whose values will, when this
rule is applied, determine the shape of each node.}
  \item{attribute.values}{A list of scalar, discrete values. For
  instance, molecule types: 'transporter', 'receptor', 'kinase'}
  \item{node.shapes}{A list of nodes selected from among those supported.}
  \item{default.shape}{A single string, the shape used if no explicit
  mapping is provided.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeColorRule
  setNodeLabelRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeShapeRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  shapes <- c ("TRIANGLE", "ROUND_RECTANGLE", "ELLIPSE")
  molecule.types <- c ("kinase", "transcription factor", "glycoprotein")
  setNodeShapeRule (cw, node.attribute.name='type', molecule.types,  shapes)
  redraw (cw)
}

\keyword{graph}
