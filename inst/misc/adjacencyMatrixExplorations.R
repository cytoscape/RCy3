# go.R
#------------------------------------------------------------------------------------------------------------------------
library (RCytoscape)
#------------------------------------------------------------------------------------------------------------------------
run = function (levels, trace=FALSE)
{
  if (0 %in% levels) {
    if (trace) print ('')
    else {
      adj.matrix <<- create.random.adjacency.matrix (dimension=3, node.names = LETTERS [1:3])
      g3 <<- new ("graphAM", adjMat = adj.matrix, edgemode="undirected")
      } # else
    } # 0

  if (1 %in% levels) {
    if (trace) print ('following the example in ?graphBAM, try graphBAM and RCy')
    else {
      source.nodes  <- c("a", "a", "b", "c", "d")
      target.nodes  <- c("b", "c", "c", "d", "a")
      weights <- c(2.3, 2.3, 4.3, 1.0, 3.0)
      df <- data.frame(from=source.nodes, to=target.nodes, weight=weights)
      g.bam <<- graphBAM (df)
      g.bam <<- initEdgeAttribute (g.bam, 'weight', 'numeric', 0.0)
      } # else
    } # 1

  if (2 %in% levels) {
    if (trace) print ('create the cy window')
    else {
      if (!exists ('cy'))
        cy <<- CytoscapeConnection ()
      window.title <<- 'graphBAM'
      if (window.title %in% as.character (getWindowList (cy)))
         destroyWindow (cy, window.title)
      cw <<- new.CytoscapeWindow (window.title, g=g.bam)
      displayGraph (cw)
      layout (cw)
      redraw (cw)
      } # else
    } # 2

  if (3 %in% levels) {
    if (trace) print ('')
    else {
      setEdgeColorRule (cw, 'weight',  c (0, 5), c ('#FFFFFF', '#FF0000'),  mode='interpolate')
      } # else
    } # 3

  if (4 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 4

  if (5 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 5

    if (6 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 6

  if (7 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 7

  if (8 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 8

  if (9 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 9

  if (10 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 10

  if (11 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 11

  if (12 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 12

  if (13 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 13

  if (14 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 14

  if (15 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 15

    if (16 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 16

  if (17 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 17

  if (18 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 18

  if (19 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 19

  if (20 %in% levels) {
    if (trace) print ('')
    else {
      } # else
    } # 20


} # run
#------------------------------------------------------------------------------------------------------------------------
create.random.adjacency.matrix = function (dimension=3, node.names = LETTERS [1:3])
{
  m = matrix (round (runif (dimension * dimension)), nrow=dimension, ncol=dimension)
  m = m + t (m)
  m [m > 1] = 1
  rownames (m) = node.names
  colnames (m) = node.names
  invisible (m)

} # create.random.adjacency.matrix 
#------------------------------------------------------------------------------------------------------------------------
test.create.random.adjacency.matrix = function ()
{
  print ('test.create.random.adjacency.matrix')
  m3 <<- create.random.adjacency.matrix (3, c ('A', 'B', 'C'))

} # test.create.random.adjacency.matrix
#------------------------------------------------------------------------------------------------------------------------
create.random.AM.graph = function (node.count)
{

} # create.random.AM.graph
#------------------------------------------------------------------------------------------------------------------------
test.create.random.AM.undirected.graph = function (node.count, edge.density = 0.5)
{
  dimension = node.count

  m = matrix (runif (dimension * dimension, 0, 1), nrow=dimension, ncol=dimension); 
  rownames (m) = LETTERS [1:dimension]
  colnames (m) = rownames (m)
  x1 <<- m
  mp = m + (edge.density/2)
  mp [mp > 1.0] = 1
  mp [mp < 1.0] = 0
  x2 <<- mp
  mps = mp + t (mp)
  mps [mps >= 1] = 1
  x3 <<- mp
  g = new ('graphAM', adjMat=mps, edgemode='undirected')

  return (list (g=g, m=m, am=mp, ams=mps))
  

} # test.create.random.AM.undirected.graph
#------------------------------------------------------------------------------------------------------------------------
extract.weights = function (m)
{

} # extract.weights
#------------------------------------------------------------------------------------------------------------------------
test.extract.weights = function ()
{
  print ('test.extract.weights')
  x <<- test.create.random.AM.undirected.graph (3)
  y <<- extract.weights ()

} # test.extract.weights
#------------------------------------------------------------------------------------------------------------------------


