\name{getLayoutNames}
\alias{getLayoutNames}
\alias{getLayoutNames,CytoscapeConnectionClass-method}
\title{getLayoutNames}
\description{
Retrieve the names of the currently supported layout algorithms.  These
may be used in subsequent calls to the 'layout' function.  Note that
some of the more attractive layout options, from yFiles, cannot be
run except from the user interface; their names do not appear here.
}
\usage{
getLayoutNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g., "jgraph-circle" "attribute-circle" "jgraph-annealing"
}

\author{Paul Shannon}


\seealso{
  getLayoutNameMapping
  getLayoutNames
  getLayoutPropertyNames
  getLayoutPropertyType
  getLayoutPropertyValue
  setLayoutProperties
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getLayoutNames (cy)
   #  [1] "jgraph-circle" "attribute-circle" "jgraph-annealing"  ...

}

\keyword{graph}
