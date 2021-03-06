% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StyleMappings.R
\name{setEdgeTargetArrowColorMapping}
\alias{setEdgeTargetArrowColorMapping}
\title{Set Edge Target Arrow Color Mapping}
\usage{
setEdgeTargetArrowColorMapping(
  table.column,
  table.column.values = NULL,
  colors = NULL,
  mapping.type = "c",
  default.color = NULL,
  style.name = NULL,
  network = NULL,
  base.url = .defaultBaseUrl
)
}
\arguments{
\item{table.column}{Name of Cytoscape table column to map values from}

\item{table.column.values}{List of values from Cytoscape table to be used in
mapping. Leave NULL to perform an automatic mapping to all column values.}

\item{colors}{List of hex colors to map to table.column.values or a color
palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
RColorBrewer::display.brewer.all()}

\item{mapping.type}{(char) continuous, discrete or passthrough (c,d,p); 
default is continuous}

\item{default.color}{Hex color to set as default}

\item{style.name}{Name of style; default is "default" style}

\item{network}{(optional) Name or SUID of the network. Default is the 
"current" network active in Cytoscape.}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is 
http://localhost:1234 and the latest version of the CyREST API supported by 
this version of RCy3.}
}
\value{
None
}
\description{
Map table column values to colors to set the target arrow color.
}
\examples{
\donttest{
setEdgeTargetArrowColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
setEdgeTargetArrowColorMapping('score', colors=paletteColorBrewerRdBu)
setEdgeTargetArrowColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
}
}
