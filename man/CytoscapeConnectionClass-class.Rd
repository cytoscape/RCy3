\name{CytoscapeConnectionClass-class}
\docType{class}
\alias{CytoscapeConnectionClass-class}
\title{Class "CytoscapeConnectionClass" }
\description{A class providing access to operations of the Cytoscape
  application which are not specific to a particular window. }
\section{Slots}{
  \describe{
    \item{\code{uri}:}{An \code{attrData} the address of the Cytoscape
      XMLRPC server.}
  }
}

\section{Methods}{
  \describe{
    \item{ping}{}
    \item{version}{}
    \item{getWindowcount}{}
    \item{getWindowID}{}
    \item{getWindowCount}{}
    \item{getWindowList}{}
    \item{destroyWindow}{}
    \item{destroyAllWindows}{}
    \item{getNodeShapes}{}
    \item{getAttributeClassNames}{}
    \item{getLineStyles}{}
    \item{getArrowShapes}{}
    \item{haveNodeAttribute}{}
    \item{haveEdgeAttribute}{}
    \item{copyNodeAttributesFromCyGraph}{}
    \item{copyEdgeAttributesFromCyGraph}{}
    \item{getGraphFromCyWindow}{}
    \item{hidePanel}{}
    \item{dockPanel}{}
    \item{floatPanel}{}
  }
}

\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())
     # create a CytoscapeConnectionClass object by calling the constructor
  cy <- CytoscapeConnection (host='localhost', port=1234)

}
\keyword{graphs}
\keyword{classes}

