\name{getVisualStyleNames}

\alias{getVisualStyleNames}
\alias{getVisualStyleNames,CytoscapeConnectionClass-method}

\title{getVisualStyleNames}
\description{
Cytoscape provides a number of canned visual styles, to which you may
add your own.  Use this method to find out the names of those which are
currently defined.
}
\usage{
getVisualStyleNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
}

\value{
a \code{list} of character strings.
}
\author{Paul Shannon}

\seealso{
  copyVisualStyle
  setVisualStyle
}


\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())


  cy = CytoscapeConnection ()
  print (getVisualStyleNames (cy))

}

\keyword{graph}
