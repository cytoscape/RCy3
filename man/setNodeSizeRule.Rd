\name{setNodeSizeRule}
\alias{setNodeSizeRule}
\alias{setNodeSizeRule,CytoscapeWindowClass-method}
\title{setNodeSizeRule}
\description{
Specify how data attributes how the specified node attribute values
determine the node size.}
\usage{
setNodeSizeRule (obj, node.attribute.name, control.points, node.sizes,mode, default.size=40)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node  attribute whose values will, when this
rule is applied, determine the size of each node.}
  \item{control.points}{A list of (currently, exactly 3) values, which
  specify the 'control points' to control the coloring of nodes}
  \item{node.sizes}{The nodes sizes which correspond to the control
    points.}
  \item{mode}{'interpolate' or 'lookup'.  This roughly corresponds to the
    visual mapping of continuously varying data (i.e., lfc or pValue), versus visual mapping
    of discrete data (i.e., molecule type, or phosphorylation status).
    With the interpolation mode, you must specify n+2 colors:  adding a 'below'
    and an 'above' color.  In lookup mode, specify exactly as many
    control.points as colors. If are data attribute values are found on
    the nodes which do not appear in  your list, they will displayed in
    the default color.}
  \item{default.size}{the size of nodes not otherwise specified.  Does not work in Cytoscape 2.7.}

}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeSizeRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'grid')
  redraw (cw)
  control.points <- c (10, 30, 80)
  node.sizes  <- c (20, 50, 80)
  node.attribute.name <- 'count'   # previously defined, has values which  range between 2 and 100
     # remind yourself of the values of count on each of the three nodes
  print (noa (getGraph (cw), 'count'))
     #  A.A   B.B   C.C 
     #  "2"  "30" "100" 
  setNodeSizeRule (cw, node.attribute.name, control.points, node.sizes, mode='interpolate')  # a warning is issued; below and above sizes must be inferred

     # now make a new rule.  explicitly specify below and above sizes
   node.sizes    <- c (1, 20, 50, 80, 200)   # anything below 20 will have size of 1; anothing above 80 will be 200.  this should be easy to see and check
   setNodeSizeRule (cw, node.attribute.name, control.points, node.sizes, mode='interpolate')  # a warning is issued; below and above sizes must be inferred

    # now use a mode='lookup' rule.  specify a size for two of the molecule types
    # look to see that the third type, glycoprotein, gets the tiny default.size of 5

  molecule.types <- c ('kinase', 'transcription factor')
  node.sizes     <- c (60, 80)
  setNodeSizeRule (cw, 'type', molecule.types,  node.sizes, default.size= 5, mode='lookup')
  redraw (cw)


}

\keyword{graph}

