#------------------------------------------------------------------------------------------------------------------------
library (RCytoscape)
library (RUnit)
#------------------------------------------------------------------------------------------------------------------------
cy = CytoscapeConnection ()   # for convenience
#------------------------------------------------------------------------------------------------------------------------
run = function (levels, trace=FALSE)
{
  if (0 %in% levels) {
    if (trace) print ('create the graph')
    else {
      nRows <<- 4
      adMatrix <<- matrix (round (runif (nRows * nRows)), ncol=nRows)
      adMatrix <<- adMatrix * upper.tri (adMatrix, diag=T)
      colnames (adMatrix) <<- 1:nRows
      g <<- new ("graphAM", adjMat = adMatrix, edgemode="directed")
      } # else
    } # 0

  if (1 %in% levels) {
    if (trace) print ('prep edge edgeInteger')
    else {
      nodeList <<- nodes (g)
      nodeListLength <<- length (nodeList)
      from <<- c ()
      to <<- c ()
      nodeInteger <<- c ()
      nodeChar <<- c ()
      nodeFloat <<- c()
      edgeInteger <<- c ()
      edgeChar <<- c ()
      edgeFloat <<- c()
      } # else
    } # 1

  if (2 %in% levels) {
    if (trace) print ('generate node and edge attributes')
    else {
      for (i in seq (1, nodeListLength)) {
        node = nodeList [i]
        nodeInteger <<- c (nodeInteger, round (runif (1, max = 500)))
        chars = paste (sample (letters), collapse="")
        nodeChar <<- c (nodeChar, chars)
        nodeFloat <<- c (nodeFloat, runif (1, max = 500))
        for (j in seq (i, nodeListLength)) {
          node2 = nodeList [j]
          if (adMatrix [i, j] == 1) {
            from <<- c (from, node)
            to <<- c (to, node2)
            edgeInteger <<- c (edgeInteger, round (runif (1, max = 500)))
            chars = paste (sample (letters), collapse="")
            edgeChar <<- c (edgeChar, chars)
            edgeFloat <<- c (edgeFloat, runif (1, max = 500))
            } # if
          } # for j
        } # for i
      } # else
    } # 2

  if (3 %in% levels) {
    if (trace) print ('add node attributes')
    else {
       g <<- initNodeAttribute (g, "nodeInteger", "integer", 0)
       nodeData (g, nodes(g), "nodeInteger") <<- nodeInteger
       g <<- initNodeAttribute (g, "nodeChar", "char", '')
       nodeData (g, nodes(g), "nodeChar") <<- nodeChar
       g <<- initNodeAttribute (g, "nodeFloat", "numeric", 0.0)
       nodeData (g, nodes(g), "nodeFloat") <<- nodeFloat
       g <<- initNodeAttribute (g, "notes", "char", "notes")
       nodeData (g, nodes(g), "notes") <<- nodeChar
      } # else
    } # 3

  if (4 %in% levels) {
    if (trace) print ('add edge attributes')
    else {
      g <<- initEdgeAttribute (g, "edgeInteger", "integer", 0)
      edgeData (g, from, to, "edgeInteger") <<- edgeInteger
      g <<- initEdgeAttribute (g, "edgeChar", "char", '')
      edgeData (g, from, to, "edgeChar") <<- edgeChar
      g <<- initEdgeAttribute (g, "edgeFloat", "numeric", 0.0)
      edgeData (g, from, to, "edgeFloat") <<- edgeFloat
      g <<- initEdgeAttribute (g, "notes", "char", "notes")
      edgeData (g, from, to,  "notes") <<- nodeChar
      } # else
    } # 4

  if (5 %in% levels) {
    if (trace) print ('create the window, display the graph')
    else {
      window.name <<- "Rich Back and Forth Test"
      if (window.name %in% as.character (getWindowList (cy)))
         destroyWindow (cy, window.name)
      cw <<- CytoscapeWindow (window.name, g)
      displayGraph (cw)
      layout (cw, "jgraph-circle")
      redraw (cw)
      } # else
    } # 5

  if (6 %in% levels) {
    if (trace) print ('retrieve the graph from cytoscape.')
    else {
      cw2 <<- existing.CytoscapeWindow (window.name, copy.graph.from.cytoscape.to.R=TRUE)
      } # else
    } # 6

  if (7 %in% levels) {
    if (trace) print ('check for equality between the original graph -- nodes, edges, and attributes -- and the one returned')
    else {
      checkEquals (sort (nodes (cw@graph)), sort (nodes (cw2@graph)))
      checkEquals (sort (edgeNames (cw@graph)),sort (edgeNames (cw2@graph)))
      original.edge.attribute.names <<- sort (eda.names (cw@graph))
      retrieved.edge.attribute.names <<- sort (eda.names (cw2@graph))
      added.attributes <<- c ('interaction', 'canonicalName', 'edgeType')
      checkEquals (sort (c (added.attributes, original.edge.attribute.names)), retrieved.edge.attribute.names)
      #checkEquals (edgeData (cw@graph, '2', '3', 'edgeChar'), edgeData (cw2@graph, '2', '3', 'edgeChar'))
      edgeNames.with.bars = sort (names (edgeData (cw@graph)))
      edgeNames.tokenized = strsplit (names (edgeData (cw@graph)), '\\|')
      edjat.names = eda.names (cw@graph)
      for (name.pair in edgeNames.tokenized) {
        a = name.pair [1]
        b = name.pair [2]
        for (ea in edjat.names) {
          g1.value = edgeData (cw@graph, a, b, ea)
          g2.value = edgeData (cw2@graph, a, b, ea)
          checkEquals (g1.value, g2.value)
          } # for ea
        } # for name.pair
      } # else
    } # 7


} # run
#------------------------------------------------------------------------------------------------------------------------
