### R code from vignette source '/Users/dtenenba/Downloads/RCy3/vignettes/RCy3.Rnw'

###################################################
### code chunk number 1: g0
###################################################
  library(RCy3)
  g <- new ('graphNEL', edgemode='directed')
  g <- graph::addNode ('A', g)
  g <- graph::addNode ('B', g)
  g <- graph::addNode ('C', g)
  cw <- CytoscapeWindow ('vignette', graph=g, overwrite=TRUE)
  displayGraph (cw)



###################################################
### code chunk number 2: g1
###################################################
  layoutNetwork (cw, layout.name='grid')
  redraw (cw)


###################################################
### code chunk number 3: g2
###################################################
  g <- cw@graph   # created above, in the section  'A minimal example'
  g <- initNodeAttribute (graph=g,  attribute.name='moleculeType',
                          attribute.type='char',
                          default.value='undefined')
  g <- initNodeAttribute (graph=g,  'lfc', 'numeric', 0.0)
  nodeData (g, 'A', 'moleculeType') <- 'kinase'
  nodeData (g, 'B', 'moleculeType') <- 'TF'
  nodeData (g, 'C', 'moleculeType') <- 'cytokine'
  nodeData (g, 'A', 'lfc') <- -1.2
  nodeData (g, 'B', 'lfc') <- 1.8
  nodeData (g, 'C', 'lfc') <- 3.2
  cw = setGraph (cw, g)
  displayGraph (cw)    # cw's graph is sent to Cytoscape
  redraw (cw)
  


###################################################
### code chunk number 4: defaults
###################################################
  setDefaultNodeShape (cw, 'OCTAGON')
  setDefaultNodeColor (cw, '#AAFF88')
  setDefaultNodeSize  (cw, 80)
  setDefaultNodeFontSize (cw, 40)
  redraw (cw)


###################################################
### code chunk number 5: g3
###################################################
  getNodeShapes (cw)   # diamond, ellipse, trapezoid, triangle, etc.
  print (noa.names (getGraph (cw)))  # what data attributes are defined?
  print (noa (getGraph (cw), 'moleculeType'))
  attribute.values <- c ('kinase',  'TF',       'cytokine')
  node.shapes      <- c ('DIAMOND', 'TRIANGLE', 'RECTANGLE')
  setNodeShapeRule (cw, node.attribute.name='moleculeType',
                    attribute.values, node.shapes)
  redraw (cw)


###################################################
### code chunk number 6: g4
###################################################
  setNodeColorRule (cw, 'lfc', c (-3.0, 0.0, 3.0),
                    c ('#00AA00', '#00FF00', '#FFFFFF', '#FF0000', '#AA0000'),
                    mode='interpolate')


###################################################
### code chunk number 7: g41
###################################################
  setNodeColorRule (cw, 'lfc', c (-3.0, 0.0, 3.0),
                    c ('#00FF00', '#FFFFFF', '#FF0000'),
                    mode='interpolate')


###################################################
### code chunk number 8: g5
###################################################
  control.points = c (-1.2, 2.0, 4.0)
  node.sizes     = c (10, 20, 50, 200, 205)
  setNodeSizeRule (cw, 'lfc', control.points, node.sizes,
                   mode='interpolate')



###################################################
### code chunk number 9: g6
###################################################
 
  g <- cw@graph
  g <- initEdgeAttribute (graph=g,  attribute.name='edgeType',
                          attribute.type='char',
                          default.value='unspecified')

  g <- graph::addEdge ('A', 'B', g)
  g <- graph::addEdge ('B', 'C', g)
  g <- graph::addEdge ('C', 'A', g)

  edgeData (g, 'A', 'B', 'edgeType') <- 'phosphorylates'
  edgeData (g, 'B', 'C', 'edgeType') <- 'promotes'
  edgeData (g, 'C', 'A', 'edgeType') <- 'indirectly activates'
  cw@graph <- g
  displayGraph (cw)

  line.styles = c ('DOT', 'SOLID', 'SINEWAVE')
  edgeType.values = c ('phosphorylates', 'promotes',
                       'indirectly activates')
  setEdgeLineStyleRule (cw, 'edgeType', edgeType.values,
                        line.styles)
  redraw (cw)

  arrow.styles = c ('Arrow', 'Delta', 'Circle')
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values,
                          arrow.styles)


###################################################
### code chunk number 10: g7
###################################################
  hidePanel (cw, 'Data Panel')
  floatPanel (cw, 'D')
  dockPanel (cw, 'd')
  hidePanel (cw, 'Control Panel')
  floatPanel (cw, 'control')
  dockPanel (cw, 'c')


###################################################
### code chunk number 11: 8a
###################################################
selectNodes(cw, 'B')


###################################################
### code chunk number 12: g8
###################################################
  getSelectedNodes (cw)


###################################################
### code chunk number 13: g9 (eval = FALSE)
###################################################
##   selectFirstNeighborsOfSelectedNodes (cw)


###################################################
### code chunk number 14: g10
###################################################
  nodes <- getSelectedNodes (cw)


###################################################
### code chunk number 15: position
###################################################
  cwe <- CytoscapeWindow ('vignette.setNodePosition', graph=RCy3::makeSimpleGraph (), overwrite=TRUE)
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  center.x <- 200
  center.y <- 200
  radius <- 200
    # sweep through full revoltion 3 times, 5 degrees at a time
  angles <- seq (0, 360, 90)
  for (angle in angles) {
    angle.in.radians <- angle * pi / 180
    x <- center.x + (radius * cos (angle.in.radians))
    y <- center.y + (radius * sin (angle.in.radians))
    setNodePosition (cwe, 'A', x, y)
    }
    # RCy will not create windows with duplicate names, so clear the decks for a subsequent possible run



###################################################
### code chunk number 16: moviePrep
###################################################
  g <- RCy3::makeSimpleGraph ()
  g <- initNodeAttribute (g, 'pval', 'numeric', 1.0)

  cwm <- CytoscapeWindow ('movie', graph=g, overwrite=TRUE)
  displayGraph (cwm)
  layoutNetwork (cwm, 'grid')
  redraw (cwm)



###################################################
### code chunk number 17: movieRules
###################################################
  lfc.control.points <- c (-3.0, 0.0, 3.0)
  lfc.colors <- c ('#00AA00', '#00FF00', '#FFFFFF', '#FF0000', '#AA0000')
  setNodeColorRule (cwm, 'lfc', lfc.control.points, lfc.colors,
                    mode='interpolate')

  pval.control.points <- c (0.1, 0.05, 0.01, 0.0001)
  pval.sizes          <- c (30, 50, 70, 100)
  setNodeSizeRule (cwm, 'pval', pval.control.points, pval.sizes,
                   mode='interpolate')



###################################################
### code chunk number 18: animate
###################################################

  pval.timepoint.1 <- c (0.01, 0.3, 0.05)
  pval.timepoint.2 <- c (0.05, 0.01, 0.01)
  pval.timepoint.3 <- c (0.0001, 0.005, 0.1)

  lfc.timepoint.1 <- c (-1.0, 1.0, 0.0)
  lfc.timepoint.2 <- c (2.0, 3.0, -2.0)
  lfc.timepoint.3 <- c (2.5, 2.0, 0.0)

  for (i in 1:5) {  # run this loop 5 times
    setNodeAttributesDirect (cwm, 'lfc',  'numeric', c ('A', 'B', 'C'),
                             lfc.timepoint.1)
    setNodeAttributesDirect (cwm, 'pval', 'numeric', c ('A', 'B', 'C'),
                             pval.timepoint.1)
    redraw (cwm)
    system ('sleep 1')

    setNodeAttributesDirect (cwm, 'lfc',  'numeric', c ('A', 'B', 'C'),
                             lfc.timepoint.2)
    setNodeAttributesDirect (cwm, 'pval', 'numeric', c ('A', 'B', 'C'),
                             pval.timepoint.2)
    redraw (cwm)
    system ('sleep 1')

    setNodeAttributesDirect (cwm, 'lfc',  'numeric', c ('A', 'B', 'C'), lfc.timepoint.3)
    setNodeAttributesDirect (cwm, 'pval', 'numeric', c ('A', 'B', 'C'), pval.timepoint.3)
    redraw (cwm)
    system ('sleep 1')
    }



###################################################
### code chunk number 19: deleteWindows
###################################################
  cy <- CytoscapeConnection ()
  window.names <- c ('vignette', 'vignette.setNodePosition', 'movie')
  for (window.name in window.names)
    if (window.name %in% as.character (getWindowList (cy)))
      deleteWindow (cy, window.name)



