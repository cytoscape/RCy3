% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StyleValues.R
\name{getNodeProperty}
\alias{getNodeProperty}
\title{Get Node Property Values}
\usage{
getNodeProperty(
  node.names = NULL,
  visual.property,
  network = NULL,
  base.url = .defaultBaseUrl
)
}
\arguments{
\item{node.names}{List of node names or SUIDs. Default is NULL for all 
nodes.}

\item{visual.property}{Name of a visual property. See \link{getVisualPropertyNames}.}

\item{network}{(optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is http://localhost:1234
and the latest version of the CyREST API supported by this version of RCy3.}
}
\value{
Named list of property values
}
\description{
Get values for any node property of the specified nodes.
}
\details{
This method retrieves the actual property of the node, given the 
current visual style, factoring together any default, mapping and bypass setting.
}
\examples{
\donttest{
getNodeProperty(c('node 0','node 1'),'NODE_SHAPE')
}
}
