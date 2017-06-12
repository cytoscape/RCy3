\name{setNodeOpacityRule}
\alias{setNodeOpacityRule}
\alias{setNodeOpacityRule,CytoscapeWindowClass-method}
\title{setNodeOpacityRule}
\description{
Specify how data attributes -- for the specified named attribute -- are
mapped to node opacity. There are two modes: 'interpolate' and 'lookup'.
In the former, you specify data values ('control points') and opacitys;
when a node's corresponding data attribute value is exactly that of a
control point, the specified opacity is used. If the node's data
attribute falls between control points, then the opacity is interpolated.
The 'lookup' mode provides no interpoloation, and is useful when you have
a node attribute with a finite set of discrete values, each of which you
you want to display in a specific opacity. For example: render all
receptors with full brightness, all transcription factors faded by 50%,
and all kinases nearly invisible.
}

\usage{
setNodeOpacityRule(obj, node.attribute.name, control.points, opacities, mode,
                   aspect='all', vizmap.style.name = 'default')
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will
        determine the opacity of each node when this rule is applied.}
  \item{control.points}{a list of values. In interpolate mode, a typical
        choice is the minimum, the maximum and some sensible midpoint.}
  \item{opacities}{a list of opacities, either two more than the number of
        control points (if mode='interpolate'), in which case the first
        opacity is used for all attributes values below the minimum, and
        the last opacity is used for those above the maximum. Or, if
        mode='lookup', the same number of opacities as control.points are
        expected. Opacities are expressed as integers in the range 0:255,
        from invisible to fully bright rendering.}
  \item{mode}{'interpolate' or 'lookup'. This roughly corresponds to the
        visual mapping of continuously varying data (i.e., lfc or pValue), versus
        visual mapping of discrete data (i.e., molecule type, or phosphorylation
        status). With the interpolation mode, you must specify n+2 opacitys:
        adding a 'below' and an 'above' opacity. In lookup mode, specify exactly
        as many control.points as opacities. If data attribute values are found
        on the nodes which do not appear in your list, they will be displayed
        in the default opacity.}
  \item{aspect}{a character string, with one or more of these values: 'border',
        'label', 'fill', 'all'}. The first three aspects describe elements of
        the displayed node: its border, its text label, and its body (or 'fill').
        'all' implies that all elements (border, label and fill) will be operated
        upon, equally, by this rule. If you want, for instance, the node label
        (its displayed name) to be visible even if the border and fill are dim,
        then use 'border, fill' as the aspect.}
  \item{vizmap.style.name}{visual style name, defaults to the style 'default'.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeColorRule, setNodeOpacityDirect
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeOpacityRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  control.points <- c(-3.0, 0.0, 3.0)   # typical range of log-fold-change ratio values
  opacities <- c(128, 80, 255)  
  setNodeOpacityRule (cw, node.attribute.name='lfc', control.points, opacities, mode='interpolate', aspect='all')
    # now restore full opacities
  gene.types <- c("kinase", "transcription factor", "glycoprotein")
  setNodeOpacityRule (cw, 'type', gene.types, c (255, 255, 255),  mode='lookup', aspect='all');
    # leaving node labels fully opaque -- fully visible -- change border and fill opacity
  opacities <- c(10, 80, 255)
  setNodeOpacityRule (cw, node.attribute.name='type', gene.types, opacities, mode='lookup', aspect='border,fill')
}

}
\keyword{graph}
