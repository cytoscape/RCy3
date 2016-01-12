\name{getLayoutPropertyType}
\alias{getLayoutPropertyType}
\alias{getLayoutPropertyType,CytoscapeConnectionClass-method}
\title{getLayoutPropertyType}
\description{
Returns a list of the tunable properties for the specified layout.
}
\usage{
getLayoutPropertyType(obj, layout.name, property.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{layout.name}{a \code{string} object. }
  \item{property.name}{a \code{string} object. }
}

\value{
  A character string specifying the type.  These types do not always
  necessarily to R types.
}

\author{Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutNameMapping
  getLayoutPropertyNames
  getLayoutPropertyValue
  setLayoutProperties
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  prop.names <- getLayoutPropertyNames (cy, 'isom')
  print (prop.names)
    # "maxEpoch"           "radiusConstantTime" "radius"             "minRadius"
    # "initialAdaptation"  "minAdaptation"      "sizeFactor"         "coolingFactor"
    # "singlePartition" 

  sapply (prop.names, function (pn) getLayoutPropertyType (cy, 'isom',   pn))
  # maxEpoch radiusConstantTime             radius          minRadius
  #           "int"              "int"              "int"              "int"
  # initialAdaptation      minAdaptation         sizeFactor      coolingFactor
  #        "double"           "double"           "double"           "double"
  # singlePartition 
  #       "boolean" 

}

\keyword{graph}
