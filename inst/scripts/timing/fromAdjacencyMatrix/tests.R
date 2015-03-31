# test.R
#------------------------------------------------------------------------------
library (RUnit)
library (RCytoscape)
library (proftools)
#------------------------------------------------------------------------------
if (!exists ('cy'))
  cy = CytoscapeConnection ()
#------------------------------------------------------------------------------
test <- function(node.count=10, edge.count=10, add.attributes=TRUE)
{
   total.time <- system.time(runTimedTest (node.count, edge.count, add.attributes))
   print('-- total time')
   print(total.time)

} # test
#------------------------------------------------------------------------------
run = function (levels)
{
  if ('demo' %in% levels) {
    Rprof ('demoSimpleGraph.out')
    window.name = 'demo.simpleGraph'
    if (window.name %in% as.character (getWindowList (cy)))
      deleteWindow (cy, window.name)
    demoSimpleGraph ()
    Rprof (NULL)
    x <<- summaryRprof('demoSimpleGraph.out')
    } # 'demo'

  if ('big' %in% levels) {
    node.count = 5000
    edge.count = 5000
    prof.file = sprintf ('profile.%d.%d.out', node.count, edge.count)
    window.name = sprintf ('demo.simpleGraph.%d.%d', node.count, edge.count)
    print(system.time(runTimedTest (node.count, edge.count, TRUE, prof.file)))
    #cwp <<- displayProfileGraph (prof.file)
    #zz <<- summaryRprof(prof.file)
    } # big

  if (1 %in% levels) {
    profile.data <<- readProfileData ('demoSimpleGraph.out')  # takes a long time.  5 minutes?
    } # 1

  if (2 %in% levels) {
    pdg <<- proftools:::np2x (profile.data, score='total')
    } # 2

  if (3 %in% levels) {
    g <<- pdgToCy (pdg)
    } # 3

  if (4 %in% levels) {
    cw <<- render (g)
    } # 4

  if (5 %in% levels) {
    viz (cw)
    } # 5

  if (6 %in% levels) {
    f = 'bigGraph005.out'
    zz <<- scan (f, sep='\n', what=character(0))
    } # 6

  if (7 %in% levels) {
    } # 7

  if (8 %in% levels) {
    } # 8

  if (9 %in% levels) {
    } # 9

  if (10 %in% levels) {
    } # 10

  if (11 %in% levels) {
    } # 11

  if (12 %in% levels) {
    } # 12

  if (13 %in% levels) {
    } # 13

  if (14 %in% levels) {
    } # 14

  if (15 %in% levels) {
    } # 15

  if (16 %in% levels) {
    } # 16

  if (17 %in% levels) {
    } # 17

  if (18 %in% levels) {
    } # 18

  if (19 %in% levels) {
    } # 19

  if (20 %in% levels) {
    } # 20


} # run
#------------------------------------------------------------------------------------------------------------------------
pdgToCy <- function (pdg)
{
  pd.nodes <<- pdg$nodes
  pd.edgeList <<- pdg$edges
  names (pd.edgeList) <<- pd.nodes
  node.times <<- eapply (profile.data$data, function (element) element$self)    

  nodes.a = unlist (sapply (names (pd.edgeList), function (source.node)
               rep (source.node, length (pd.edgeList [[source.node]]))), use.names=FALSE)
  nodes.b = unlist (pd.edgeList, use.names=F)
  g = new("graphNEL", edgemode = "directed")
  g = initNodeAttribute(g, "self.time", "integer", "0")
  g = initNodeAttribute(g, "total.time", "integer", "0")
  g = initNodeAttribute(g, "label", "char", "")
  g = graph::addNode(pd.nodes, g)
  g = graph::addEdge(nodes.a, nodes.b, g)
  node.names.in.time.order = intersect (pd.nodes, names (node.times))
  time.data = mget (node.names.in.time.order, profile.data$data)
  self.times = as.integer (sapply (time.data, function (e) e$self))
  total.times = as.integer (sapply (time.data, function (e) e$total))
  nodeData (g, node.names.in.time.order, 'self.time') = self.times
  nodeData (g, node.names.in.time.order, 'total.time') = total.times
  nodeData (g, node.names.in.time.order, 'label') = node.names.in.time.order
  gt <<- g
  return (g)

} # pdgToCy
#------------------------------------------------------------------------------------------------------------------------
render <- function (g)
{
  cw <- new.CytoscapeWindow('filterVCF times', gt, overwriteWindow=TRUE)
  displayGraph(cw)

  return (cw)

} # render
#------------------------------------------------------------------------------------------------------------------------
viz <- function (cw)
{
  #layoutNetwork(cw, "jgraph-spring")
  setNodeLabelRule(cw, "label")
  setDefaultNodeFontSize (cw, 24)
  showGraphicsDetails (cw, TRUE)
  #setNodeSizeRule (cw, 'total.time', c (0, 1, 2, 16, 575), c (10, 20, 30, 80, 300), mode='interpolate')
  setNodeSizeRule (cw, 'self.time', c (0, 2400), c (10, 300), mode='interpolate')
  setNodeTooltipRule (cw, 'label')
  setTooltipInitialDelay (cw, 10)
  setTooltipDismissDelay (cw, 10000)

  redraw (cw)
  restoreLayout (cw, 'layout.RData')

  return (cw)

} # viz
#------------------------------------------------------------------------------------------------------------------------
makeAdjacencyMatrix = function (node.count, edge.count)
{
    # generate node.count^2 uniform random numbers, between 0 and 1
  vec = runif (node.count * node.count)  
  z <<- vec
    # fit this vec into a matrix, set lower.tri to zero, keep diagonal
  m0 <<- matrix (vec, ncol=node.count)
  m0 [lower.tri (m0)] <<- 0

    # now convert back to a vector so more elements can be eliminated
  vec2 <<- as.numeric (m0)

    # what element value will allows us to keep 'edge.count' edges only, in this upper.tri matrix?
  threshold = tail (sort (vec2), n=edge.count) [1]

    # use the threshold to identify elements to keep, elements to eliminate by seting them to zero
  keepers <<- which (vec2 >= threshold)
  removers = setdiff (1:(node.count * node.count), keepers)
  vec2 [keepers] = 1
  vec2 [removers] = 0
  x <<- vec2
  stopifnot (sum (vec2) == edge.count)

    # now put the much-manipulated vector, all zeroes and ones, back into an upper.tri matrix with diagonal
    # stuff the vector back in 'by column' -- apparently because 'as.numeric' gets them out that way
  adMatrix <<- matrix (vec2, ncol=node.count, byrow=F)
  colnames (adMatrix) <<- 1:node.count
  rownames (adMatrix) <<- 1:node.count

  return (adMatrix)

} # makeAdjacencyMatrix
#------------------------------------------------------------------------------------------------------------------------
testMakeAdjacencyMatrix = function ()
{
  print ('--- testMakeAdjacencyMatrix')

  nodes = 5; edges = 3;
  m1 <<- make.adjacency.matrix (nodes, edges)
  checkEquals (nrow (m1), nodes)
  checkEquals (sum (m1), edges)

  nodes = 5; edges = 0;
  m1 <<- make.adjacency.matrix (nodes, edges)
  checkEquals (nrow (m1), nodes)
  checkEquals (sum (m1), edges)

  nodes = 50; edges = 38;
  m1 <<- make.adjacency.matrix (nodes, edges)
  checkEquals (nrow (m1), nodes)
  checkEquals (sum (m1), edges)



} # testMakeAdjacencyMatrix
#------------------------------------------------------------------------------------------------------------------------
makeGraphFromAdjacencyMatrix = function (matrix, add.attributes=FALSE)
{
  g = new ("graphAM", adjMat = matrix, edgemode="directed")
  g = initNodeAttribute (g, 'label', 'char', 'noLabel')
  nodeData (g, nodes (g), 'label') = nodes (g)

  node.count = length (nodes (g))
  stopifnot (node.count == nrow (matrix))

  if (add.attributes) {  # add node attributes
    g = initNodeAttribute (g, "integer.noa", "integer", 0)
    nodeData (g, nodes(g), "integer.noa") = round (runif (node.count, 0, 1000))
    g = initNodeAttribute (g, "char.noa", "char", '')
    nodeData (g, nodes(g), "char.noa") = sample (letters, node.count, replace=T)
    g = initNodeAttribute (g, "numeric.noa", "numeric", 0.0)
    nodeData (g, nodes(g), "numeric.noa") = runif (node.count, 0, 1000)
    } # add.attributes

  if (add.attributes) {  # add edges attributes
     idx <- which(matrix==1)
     idx0 <- idx-1
     row.indices <- idx0 %% nrow(matrix) + 1L
     col.indices <- idx0 %/% nrow(matrix) + 1L
     source.nodes <- rownames(matrix) [row.indices]
     target.nodes <- colnames(matrix) [col.indices]
     stopifnot(length(source.nodes) == length(target.nodes))
     edge.count <- length(source.nodes)
     g <- makeGraphFromAdjacencyMatrix(matrix, add.attributes=FALSE)
     g = initEdgeAttribute(g, "char.eda", "char", '')
     g = initEdgeAttribute(g, "numeric.eda", "numeric", 0.0)
     g = initEdgeAttribute(g, "integer.eda", "integer", 0)
     
     
     integer.eda <- as.integer(round(runif(edge.count, 0, 1000)))
     numeric.eda <- runif(edge.count, 0, 1000)
     char.eda <- sample(letters, size=edge.count, replace=TRUE)
     
     edgeData(g, source.nodes, target.nodes, 'integer.eda') <- integer.eda
     edgeData(g, source.nodes, target.nodes, 'numeric.eda') <- numeric.eda
     edgeData(g, source.nodes, target.nodes, 'char.eda') <- char.eda
     } # add.attributes

  as(g, 'graphNEL')

} # makeGraphFromAdjacencyMatrix
#------------------------------------------------------------------------------------------------------------------------
testMakeGraphFromAdjacencyMatrix = function ()
{
  print ('--- testMakeGraphFromAdjacencyMatrix')

  m2 <<- makeAdjacencyMatrix (4, 2)
  g2 <<- makeGraphFromAdjacencyMatrix (m2, add.attributes=FALSE)

  checkEquals (length (nodes (g2)), 4)
  checkEquals (length (edgeNames (g2)), 2)

  g3 <<- makeGraphFromAdjacencyMatrix (m2, add.attributes=T)
  checkEquals (length (nodes (g3)), 4)
  checkEquals (length (edgeNames (g3)), 2)

  checkEquals (sort (noa.names (g3)), c ("char.noa", "integer.noa", "numeric.noa"))
  checkEquals (sort (eda.names (g3)), c ("char.eda", "integer.eda", "numeric.eda"))

  checkTrue (all (as.character (noa (g3, 'char.noa')) %in% letters))
  checkTrue (all (as.integer   (noa (g3, 'integer.noa'))) %in% 0:1000)
  checkTrue (all (is.numeric   (noa (g3, 'numeric.noa'))))

  m.big <<- makeAdjacencyMatrix (1000, 500)
  g.big <<- makeGraphFromAdjacencyMatrix (m.big, add.attributes=F)
  checkEquals (length (nodes (g.big)), 1000)
  checkEquals (length (edgeNames (g.big)), 500)
  
  
} # testMakeGraphFromAdjacencyMatrix 
#------------------------------------------------------------------------------------------------------------------------
runTimedTest = function (node.count=10, edge.count=8, add.attributes=FALSE, prof.file)
{
  print ('-- matrix')
  print(system.time((m <- makeAdjacencyMatrix (node.count, edge.count))))
  print ('-- graph')
  print(system.time((g <- makeGraphFromAdjacencyMatrix (m, add.attributes))))

  window.title = sprintf ('timing: %d nodes %d edges', node.count, edge.count)
  print ('-- cw')
  print(system.time((cw <- new.CytoscapeWindow (window.title, g, overwriteWindow=TRUE))))
  print ('-- display')
  print(system.time(displayGraph (cw)))
  
  ##print ('-- layout')
  ##print(system.time((layoutNetwork (cw, 'jgraph-circle'))))
  print('-- redraw')
  print (system.time(redraw (cw)))

} # runTimedTest
#------------------------------------------------------------------------------------------------------------------------
displayProfileGraph = function (profileName)
{
  profile.data <<- readProfileData (profileName)  # takes a long time.  5 minutes?
  pdg <<- proftools:::np2x (profile.data, score='self')
  printf ('n2px returned graph with %d nodes', length (pdg$nodes))
  g <- pdgToCy (pdg)
  printf ('pdgToCy returned graph with %d nodes', length (nodes (g)))
  cwp <- new.CytoscapeWindow(profileName, g, overwriteWindow=TRUE)
  displayGraph(cwp)
  return (viz (cwp))

} # displayProfileGraph
#------------------------------------------------------------------------------------------------------------------------
simple.test <- function ()
{
  url = "http://xmlrpc-c.sourceforge.net/api/sample.php"
  xml.rpc (url, 'system.listMethods')
  xml.rpc (url, 'sample.add', 3L, 4L)  # [1] 7
  Rprof ('test.prof', memory.profiling=FALSE, interval=0.01)
  xml.rpc (url, 'sample.add', i:1000)
  Rprof (NULL)
  summaryRprof ('test.prof')

} # simple.test
#------------------------------------------------------------------------------------------------------------------------
# load('g-5000-5000.RData')
# m <- makeAdjacencyMatrix(10,10)
# idx <- which(m==1)
# idx0 <- idx-1
# row.indices <- idx0 %% nrow(m) + 1L
# col.indices <- idx0 %/% nrow(m) + 1L
# source.nodes <- rownames(m) [row.indices]
# target.nodes <- colnames(m) [col.indices]
# stopifnot(length(source.nodes) == length(target.nodes))
# edge.count <- length(source.nodes)
# g <- makeGraphFromAdjacencyMatrix(m, add.attributes=FALSE)
# g = initEdgeAttribute(g, "char.eda", "char", '')
# g = initEdgeAttribute(g, "numeric.eda", "numeric", 0.0)
# g = initEdgeAttribute(g, "integer.eda", "integer", 0)
# 
# 
# integer.eda <- as.integer(round(runif(edge.count, 0, 1000)))
# numeric.eda <- runif(edge.count, 0, 1000)
# char.eda <- sample(letters, size=edge.count, replace=TRUE)
# 
# edgeData(g, source.nodes, target.nodes, 'integer.eda') <- integer.eda
# edgeData(g, source.nodes, target.nodes, 'numeric.eda') <- numeric.eda
# edgeData(g, source.nodes, target.nodes, 'char.eda') <- char.eda
# 
# int.attributes <- 
#     for (i in 1:node.count) 
#       for (j in 1:node.count)
#         if (matrix [i,j] == 1) {
#            node.a = as.character (i)
#            node.b = as.character (j)
#            edgeData (g, node.a, node.b, 'integer.eda') = round (runif (1, 0, 1000))
#            edgeData (g, node.a, node.b, 'numeric.eda') = runif (1, 0, 1000)
#            edgeData (g, node.a, node.b, 'char.eda') = sample (letters, 1)
#            } # if an edge between i and j
#
