\name{getLayoutNameMapping}
\alias{getLayoutNameMapping}
\alias{getLayoutNameMapping,CytoscapeConnectionClass-method}
\title{getLayoutNameMapping}
\description{
The Cytoscape 'Layout' menu lists many layout algorithms, but the names
presented there are different from the names by which these
algorithms are known to layout method.   This method returns a
named list in which the names are from the GUI, and the values identify
the names you must use to choose an algorithms in the programmatic interface.
}
\usage{
getLayoutNameMapping(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
 A named list of strings.
}

\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  layout
  getLayoutNames
  getLayoutPropertyNames
  getLayoutPropertyType
  getLayoutPropertyValue
  setLayoutProperties
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  layout.name.map <- getLayoutNameMapping (cy)
  print (head (names (layout.name.map), n=3))
    # [1] "Attribute Circle Layout"     "Stacked Node Layout"        
    # [3] "Degree Sorted Circle Layout"
  print (head (as.character (layout.name.map), n=3))
    # [1] "attribute-circle"    "stacked-node-layout" "degree-circle"
}

\keyword{graph}
