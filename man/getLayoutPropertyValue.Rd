\name{getLayoutPropertyValue}
\alias{getLayoutPropertyValue}
\alias{getLayoutPropertyValue,CytoscapeConnectionClass-method}
\title{getLayoutPropertyValue}
\description{
Returns the appropriately typed value of the specified
tunable property for the specified layout.
}
\usage{
getLayoutPropertyValue(obj, layout.name, property.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{layout.name}{a \code{string} object. }
  \item{property.name}{a \code{string} object. }
}

\value{
  Typically an integer, numeric or string value, the current setting of
  this property for this layout.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutNameMapping
  getLayoutPropertyNames
  getLayoutPropertyType
  setLayoutProperties
 }

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  prop.names <- getLayoutPropertyNames (cy, 'isom')
  print (prop.names)
    # "maxEpoch"           "radiusConstantTime" "radius"             "minRadius"
    # "initialAdaptation"  "minAdaptation"      "sizeFactor"         "coolingFactor"
    # "singlePartition" 
  sapply (prop.names, function (pn) getLayoutPropertyValue (cy, 'isom', 'coolingFactor'))
    # maxEpoch radiusConstantTime             radius          minRadius 
    #       2                  2                  2                  2 
    # initialAdaptation      minAdaptation         sizeFactor      coolingFactor 
    #               2                  2                  2                  2 
    # singlePartition 
    #               2 
}

}
\keyword{graph}
