\name{getLayoutPropertyType}
\alias{getLayoutPropertyType}
\alias{getLayoutPropertyType,CytoscapeConnectionClass-method}
\title{getLayoutPropertyType}
\description{
Returns the type of one of the tunable properties
(property.name) for the specified layout.
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
  A character string specifying the type. These types do not always
  necessarily need to be R types.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutNameMapping
  getLayoutPropertyNames
  getLayoutPropertyValue
  setLayoutProperties
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  prop.names <- getLayoutPropertyNames (cy, 'isom')
  print (prop.names)
    # "maxEpoch"           "radiusConstantTime" "radius"             "minRadius"
    # "initialAdaptation"  "minAdaptation"      "sizeFactor"         "coolingFactor"
    # "singlePartition" 

  sapply (prop.names, function (pn) getLayoutPropertyType (cy, 'isom', pn))
    # maxEpoch radiusConstantTime             radius          minRadius
    #           "int"              "int"              "int"              "int"
    # initialAdaptation      minAdaptation         sizeFactor      coolingFactor
    #        "double"           "double"           "double"           "double"
    # singlePartition 
    #       "boolean" 
}

}
\keyword{graph}
