\name{getLayoutPropertyNames}
\alias{getLayoutPropertyNames}
\alias{getLayoutPropertyNames,CytoscapeConnectionClass-method}
\title{getLayoutPropertyNames}
\description{
Returns a list of the tunable properties for the specified layout.
}
\usage{
getLayoutPropertyNames(obj, layout.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{layout.name}{a \code{string} object. }
}

\value{
 A named list of strings.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutNameMapping
  getLayoutPropertyType
  getLayoutPropertyValue
  setLayoutProperties
 }

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  prop.names <- getLayoutPropertyNames (cy, 'isom')
  print (prop.names)
    # [1] "maxEpoch"           "radiusConstantTime" "radius"             "minRadius"         
    # [5] "initialAdaptation"  "minAdaptation"      "sizeFactor"         "coolingFactor"     
    # [9] "singlePartition"  
}

}
\keyword{graph}
