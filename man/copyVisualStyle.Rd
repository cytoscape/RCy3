\name{copyVisualStyle}

\alias{copyVisualStyle}
\alias{copyVisualStyle,CytoscapeConnectionClass-method}

\title{copyVisualStyle}

\description{
Once you have designed a visual style, you may wish to duplicate it,
perhaps in preparation for adding further mapping rules.  Another
scenario arises when style rules have been added to the 'default' style,
and you wish to create a Cytoscape session file with your current
network and this default style.  However, the default style is not saved
into a session, only explicitly named styles are.  Use this method to
achieve this.
}
\usage{
copyVisualStyle(obj, from.style, to.style)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
  \item{from.style}{a \code{character string} specifying the name of an
    existing style you wish to copy }
  \item{to.style}{a \code{character string}, the name of an as yet non-existent style}
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  getVisualStyleNames
  setVisualStyle
}

\examples{

    # create the usual demo graph and Cytoscape window, then
    # specify that all the edges should be 5 pixels wide.  This affects the 'default' style only
    # in order to save this style for later use, copy it to a 
    # new style named 'fatEdgeStyle'
    # the related method 'setVisualStyle' must be called in order for
    # the fatEdgestyle to be associated with this window (and saved
    # into the CytoscapeSession file from the Cytoscape application's
    # File menu)

  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.copyVisualStyle'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  setDefaultEdgeLineWidth (cw, 5);
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)

     # create a unique style name, using millisecond precision, so should be unique
  time.msec = proc.time()[['elapsed']]
  new.unique.style.name = paste ('fatEdgeStyle', time.msec, sep='.')

  copyVisualStyle (cw, 'default', new.unique.style.name) 
  new.names = getVisualStyleNames (cw)
  setVisualStyle (cw, new.unique.style.name)

   # save the session form the Cytoscape application menu.  the new
   # style name  will be saved along with the network and its attributes
}

\keyword{graph}
