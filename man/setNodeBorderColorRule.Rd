\name{setNodeBorderColorRule}
\alias{setNodeBorderColorRule}
\alias{setNodeBorderColorRule,CytoscapeWindowClass-method}
\title{setNodeBorderColorRule}
\description{
Specify how data attributes -- for the specified named attribute -- are
mapped to node color.  There are two modes:  'interpolate' and
'lookup'.  In the former, you specify data values ('control points') and
colors; when a node's corresponding data attribute value is exactly that
of a control point, the specified color is used.  If the node's data
attribute falls between control points, then the color is interpolated.
Note!  In the 'interpolate' mode, you almost always want to provide two additional
colors: one for node data values falling below the minimum control
point, one for node data values falling above the maximum control
point.  If you provide an equal number of colors and control.points, the
default.color is used to paint nodes above and below the specified
range.  A useful data exploration strategy would be to use
\code{default.color <- '#000000'} causing all extreme nodes to be
painted black.

The 'lookup' mode provides no interpoloation, and is useful when  you have
a node attribute with a finite set of discrete values, each of which you
you want to display in a specific color.  For example:  render all
receptors in yellow, all transcription factors in blue, and all kinases in
dark red.
}

\usage{
setNodeBorderColorRule(obj, node.attribute.name, control.points, colors, mode, default.color='#000000')
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will, when this
rule is applied, determine the color of each node.}
  \item{control.points}{a list of values.  In the interpolate mode, a typical choice is the minimum, the maximum, some sensible midpoint.}
  \item{colors}{a list of colors, either two more than the number of
    control points (if mode='interpolate'), in which case the first color
    is used for all attributes values below the minimum, and the
    last color is used for those above the maximum.   Or, if
    mode='lookup', the same number of colors as control.points are
    expected.  Colors are expressed as quoted hexadecimal RGB strings, e.g., '#FF0000' or '#FA8800'}
  \item{mode}{'interpolate' or 'lookup'.  This roughly corresponds to the
    visual mapping of continuously varying data (i.e., lfc or pValue), versus visual mapping
    of discrete data (i.e., molecule type, or phosphorylation status).
    With the interpolation mode, you must specify n+2 colors:  adding a 'below'
    and an 'above' color.  In lookup mode, specify exactly as many
    control.points as colors. If are data attribute values are found on
    the nodes which do not appear in  your list, they will displayed in
    the default color.}
    \item{default.color}{'#000000' (black) by default, to catch your
      eye.  Used primarily in mode=='lookup' and in mode='interpolate'
      if you fail to specify 'above' and 'below' values.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeShapeRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeBorderColorRule.test', graph=makeSimpleGraph())
  control.points <- c (-3.0, 0.0, 3.0)   # typical range of log-fold-change ratio values
    # paint negative values shades of green, positive values shades of
    # red, out-of-range low values are dark green; out-of-range high
    # values are dark red
  colors <- c ("#00AA00", "#00FF00", "#FFFFFF", "#FF0000", "#AA0000")
  setNodeBorderColorRule (cw, node.attribute.name='lfc', control.points, colors, mode='interpolate')
  redraw (cw)
  data.values <- c ("kinase", "transcription factor", "glycoprotein")
  colors <- c ("#0000AA", "#FFFF00", "#0000AA") 
  setNodeBorderColorRule (cw, node.attribute.name='type', data.values,   colors, mode='lookup', default.color='#AA0000')
}


\keyword{graph}
