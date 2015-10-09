\name{getAttributeClassNames}
\alias{getAttributeClassNames}
\alias{getAttributeClassNames,CytoscapeConnectionClass-method}
\title{getAttributeClassNames}
\description{
Retrieve the names of the recognized and supported names for the class
of any node or edge attribute.  Two or three options are provided for
each of the basic types, with the intention that you can use names that
seem natural to you, and RCytoscape will recognize them.  
}
\usage{
getAttributeClassNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings group, e.g., "floating|numeric|double",
"integer|int", "string|char|character"  
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cyw <- CytoscapeConnection ()
  getAttributeClassNames (cyw)
   # [1] "floating|numeric|double" "integer|int"             "string|char|character"  
}

\keyword{graph}
