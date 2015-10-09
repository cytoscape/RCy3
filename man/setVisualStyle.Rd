\name{setVisualStyle}

\alias{setVisualStyle}
\alias{setVisualStyle,CytoscapeConnectionClass-method}

\title{setVisualStyle}

\description{
Cytoscape provides a number of canned visual styles.  You can also
create your own.   Use this method to establish an (already-defined)
visual style  as the style which governs the display of a network in the
specified CytoscapeWindow object.
}
\usage{
setVisualStyle(obj, new.style.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.style.name}{a \code{character string} specifying the name of
    an existing style you wish to use. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  getVisualStyleNames
  copyVisualStyle
}



\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.setVisualStyle'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)

  styles = getVisualStyleNames (cw)
   # now cycle through the currently defined styles
  for (style in styles) {
    print (paste ("about to set new style:", style))
    setVisualStyle (cw, style)
    }
}

\keyword{graph}
