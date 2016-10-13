\name{setLayoutProperties}
\alias{setLayoutProperties}
\alias{setLayoutProperties,CytoscapeConnectionClass-method}
\title{setLayoutProperties}
\description{
Sets the specified properties for the specified layout. Unmentioned
properties are left unchanged.
}
\usage{
setLayoutProperties(obj, layout.name, properties.list)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{layout.name}{a \code{string} object. }
  \item{properties.list}{a \code{a named list} with as many entries as
    you wish to modify }
}

\value{
None.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutNameMapping
  getLayoutPropertyNames
  getLayoutPropertyType
  getLayoutPropertyValue
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  prop.names <- getLayoutPropertyNames (cy, 'isom')
  print (prop.names)
    # [1] "maxEpoch"           "radiusConstantTime" "radius"             "minRadius"         
    # [5] "initialAdaptation"  "minAdaptation"      "sizeFactor"         "coolingFactor"     
    # [9] "singlePartition"  
  print (getLayoutPropertyValue (cy, 'isom', 'radiusConstantTime'))
    # [1] 4
    
    # modify just two of the eight properties; the others are unchanged
  setLayoutProperties (cy, 'isom', list (radiusConstantTime=4, radius=20))
}

}
\keyword{graph}
