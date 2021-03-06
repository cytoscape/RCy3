% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StyleMappings.R
\name{setNodeHeightMapping}
\alias{setNodeHeightMapping}
\title{Set Node Height Mapping}
\usage{
setNodeHeightMapping(
  table.column,
  table.column.values = NULL,
  heights = NULL,
  mapping.type = "c",
  default.height = NULL,
  style.name = NULL,
  network = NULL,
  base.url = .defaultBaseUrl
)
}
\arguments{
\item{table.column}{Name of Cytoscape table column to map values from}

\item{table.column.values}{List of values from Cytoscape table to be used 
in mapping. Leave NULL to perform an automatic mapping to all column values.}

\item{heights}{List of height values to map to table.column.values. A range
of 10 to 100 is used by default for automatic mapping.}

\item{mapping.type}{(char) continuous, discrete or passthrough (c,d,p); default is continuous}

\item{default.height}{Size value to set as default}

\item{style.name}{Name of style; default is "default" style}

\item{network}{(optional) Name or SUID of the network. Default is the "current" 
network active in Cytoscape.}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is http://localhost:1234
and the latest version of the CyREST API supported by this version of RCy3.}
}
\value{
None
}
\description{
Map table column values to the node heights.
}
\details{
Using this function will unlock node width and height to use  
separate values.
}
\examples{
\donttest{
setNodeHeightMapping('score')
setNodeHeightMapping('score', heights=c(30,80))
setNodeHeightMapping('score', c(0,30), c(35,55))
}
}
