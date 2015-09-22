\name{msg}
\alias{msg}
\alias{msg,CytoscapeConnectionClass-method}
\title{msg}
\description{
Display the supplied string in the Cytoscape Desktop status bar
}
\usage{
msg(obj, string)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{string}{a \code{char}, an arbitrary string, which can be used to inform the user of things they may wish to know}
}

\value{
Nothing.
}
\author{Paul Shannon}


\seealso{
  clearMsg
}

\examples{
  cy <- CytoscapeConnection ()
  msg (cy, 'this message will appear in the Cytoscape Desktop
  status bar, which is found at the lower corner of the Desktop')
}


\keyword{graph}
