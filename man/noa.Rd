\name{noa}
\alias{noa}
\title{
noa}
\description{
Retrieve the value of the specified node attribute for every node in the
graph.
}
\usage{
noa(graph, node.attribute.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{typically, a bioc graphNEL)}
  \item{node.attribute.name}{a character string}
}
\value{
A list, the contents of which are the attribute values, the names of
which are the names of the nodes.
}
\author{
Paul Shannon
}
\seealso{
noa.names
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g <- makeSimpleGraph ()
  noa (g, 'type')
    #           A.A                    B.B                    C.C 
    #      "kinase" "transcription factor"         "glycoprotein" 

}
\keyword{graph}
