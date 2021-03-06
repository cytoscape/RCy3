% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StyleDefaults.R
\name{setNodeCustomRingChart}
\alias{setNodeCustomRingChart}
\title{Set Node Custom Ring Chart}
\usage{
setNodeCustomRingChart(
  columns,
  colors = NULL,
  startAngle = 0,
  holeSize = 0.5,
  slot = 1,
  style.name = NULL,
  base.url = .defaultBaseUrl
)
}
\arguments{
\item{columns}{List of node column names to be displayed.}

\item{colors}{(optional) List of colors to be matched with columns or with
range, depending on type. Default is a set of colors from an appropriate 
Brewer palette.}

\item{startAngle}{(optional) Angle to start filling ring Default is 0.0.}

\item{holeSize}{(optional) Size of hole in ring. Ranges 0-1. Default is 0.5.}

\item{slot}{(optional) Which custom graphics slot to modify. Slots 1-9 are 
available for independent charts, gradients and images. Default is 1.}

\item{style.name}{(optional) Name of style; default is "default" style}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is http://localhost:1234
and the latest version of the CyREST API supported by this version of RCy3.}
}
\value{
None
}
\description{
Makes a ring chart per node using specified node table columns by
setting a default custom graphic style.
}
\examples{
\donttest{
setNodeCustomRingChart(c("data1","data2","data3"))
}
}
\seealso{
setNodeCustomPosition, removeNodeCustomGraphics
}
