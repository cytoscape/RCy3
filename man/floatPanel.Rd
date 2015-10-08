\name{floatPanel}
\alias{floatPanel}
\alias{floatPanel,CytoscapeConnectionClass-method}
\title{floatPanel}
\description{
The specified panel will 'float' detached from its 'home' position in the Cytoscape
Desktop.  As of this writing (10 aug 2010) the panel will tenaciously
claim the topmost (visual) position on the screen...  The
\code{panelName}  parameter is very flexible:  a match is defined as a
case-independent match of the supplied panelName to any starting
characters in the actual panelName.  Thus, 'd' and 'DA' both identify 'Data Panel'.
}
\usage{
floatPanel(obj, panelName)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{panelName}{a character string, providing a partial or complete
    case-independent match to the start of the name of an actual panel. }
}

\value{
Nothing.
}
\author{Paul Shannon}
\seealso{
  hidePanel
  dockPanel
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy  <- CytoscapeConnection ()
  floatPanel (cy, 'Control Panel')
    # or with less typing
  floatPanel (cy, 'c')
}

\keyword{graph}
