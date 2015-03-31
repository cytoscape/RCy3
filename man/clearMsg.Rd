\name{clearMsg}
\alias{clearMsg}
\alias{clearMsg,CytoscapeConnectionClass-method}
\title{clearMsg}
\description{
Clears any current message in the Cytoscape Desktop status bar.
}
\usage{
clearMsg(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  msg
}



\examples{
  cy <- CytoscapeConnection ()
  clearMsg (cy)
}


\keyword{graph}
