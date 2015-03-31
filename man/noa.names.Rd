\name{noa.names}
\alias{noa.names}
\title{
noa.names
}
\description{
Retrieve the names of the node attributes in the specified graph.
}
\usage{
noa.names(graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
}
\author{
Paul Shannon
}

\seealso{
  noa, eda, eda.names
}

\examples{
  g <- makeSimpleGraph()
  noa.names (g)
    #  [1] "type"  "lfc"   "label" "count"
}



\keyword{graph}
