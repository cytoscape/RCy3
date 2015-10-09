\name{getDirectlyModifiableVisualProperties}
\alias{getDirectlyModifiableVisualProperties}
\alias{getDirectlyModifiableVisualProperties,CytoscapeConnectionClass-method}
\title{getDirectlyModifiableVisualProperties}
\description{
Retrieve the names of those visual attributes which can be set directly,
bypassing vizmap rules.
}
\usage{
getDirectlyModifiableVisualProperties(obj, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{vizmap.style.name}{a visual style name. }
}

\value{
A list of about 99 character strings, e.g., "EDGE_LABEL_FONT_SIZE" and "NODE_SHAPE"
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeConnection ()
  getDirectlyModifiableVisualProperties (cw, vizmap.style.name="default")


}

\keyword{graph}
