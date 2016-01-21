\name{hidePanel}
\alias{hidePanel}
\alias{hidePanel,CytoscapeConnectionClass-method}
\title{hidePanel}
\description{
The specified panel will be hidden, and no longer visible in the Cytoscape
Desktop of, if floating, elsewhere on the computer screen.  The
\code{panelName}  parameter is very flexible:  a match is defined as a
case-independent match of the supplied panelName to any starting
characters in the actual panelName.  Thus, 'd' and 'DA' both identify
'Data Panel'. Possible options also include: 'WEST', 'EAST', 'SOUTH',
'SOUTH_WEST'. The 'SOUTH' panel is the Data Panel and the 'WEST' panel
is the control panel.
}
\usage{
hidePanel(obj, panelName)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{panelName}{a character string, providing a partial or complete
    case-independent match to the start of the name of an actual panel. }
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  floatPanel
  dockPanel
  hideAllPanels
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  hidePanel (cy, 'Control Panel')
   # or
  hidePanel (cy, 'c')
    # or
  hidePanel (cy, 'WEST')
}

\keyword{graph}
