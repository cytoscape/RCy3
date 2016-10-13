\name{setNodeShapeRule}
\alias{setNodeShapeRule}
\alias{setNodeShapeRule,CytoscapeWindowClass-method}
\title{setNodeShapeRule}
\description{
Specify how data attributes -- the specified node attribute values --
determine the node shape.}
\usage{
setNodeShapeRule (obj, node.attribute.name=, attribute.values,
node.shapes, default.shape)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will
        determine the shape of each node when this rule is applied.}
  \item{attribute.values}{A list of scalar, discrete values. For
        instance, molecule types: 'transporter', 'receptor', 'kinase'}
  \item{node.shapes}{A list of nodes selected from among those supported.}
  \item{default.shape}{A single \code{String}, the shape used if no explicit
        mapping is provided.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeColorRule
  setNodeLabelRule
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeShapeRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  shapes <- c("TRIANGLE", "ROUND_RECTANGLE", "ELLIPSE")
  molecule.types <- c("kinase", "transcription factor", "glycoprotein")
  setNodeShapeRule (cw, node.attribute.name='type', molecule.types, shapes)
}

}
\keyword{graph}
