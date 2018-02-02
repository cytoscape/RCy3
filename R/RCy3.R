#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

#-----------------------------------------------------------
# methods related to transmitting data from Cytoscape to R
#-----------------------------------------------------------
setGeneric ('createGraphFromNetwork',           signature='obj', function (obj, title=NA) standardGeneric ('createGraphFromNetwork'))
setGeneric ('connectToNewestCyWindow',       signature='obj', function (obj=CytoscapeConnection(), copyToR = FALSE) standardGeneric('connectToNewestCyWindow'))

#-----------------------------------------------------------
# methods related to transmitting data from obj@graph to 
# Cytoscape thus requiring a full CytoscapeWindowClass obj
#-----------------------------------------------------------
setGeneric ('sendNetworkFromGraph',               function (obj) standardGeneric('sendNetworkFromGraph'))
setGeneric ('displayGraph',               signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('predictTimeToDisplayGraph',  signature='obj', function (obj) standardGeneric ('predictTimeToDisplayGraph'))
setGeneric ('setGraph', 	              signature='obj', function (obj, graph) standardGeneric ('setGraph'))
setGeneric ('getGraph', 	              signature='obj', function (obj) standardGeneric ('getGraph'))
setGeneric ('sendNodeAttributesFromGraph',signature='obj', function (obj, attribute.name) standardGeneric ('sendNodeAttributesFromGraph'))
setGeneric ('sendEdgeAttributesFromGraph',signature='obj', function (obj, attribute.name) standardGeneric ('sendEdgeAttributesFromGraph'))
setGeneric ('sendNodesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendNodesFromGraph'))
setGeneric ('sendEdgesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendEdgesFromGraph'))
setGeneric ('sendEdgesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendEdgesFromGraph'))

setGeneric ('cyPlot',                                  function (node.df, edge.df) standardGeneric('cyPlot'))

# ------------------------------------------------------------------------------
CytoscapeWindowFromNetwork = 
    function(obj, title=NA, return.graph=FALSE) {
        
        if(missing(obj))
            obj<-CytoscapeConnection()

        # if title=NA, will return current network in Cytoscape
		existing.suid = as.character(getNetworkSuid(obj,title))
		
		# inform user if the window does not exist
        if (is.na(existing.suid)) {
            write(sprintf("ERROR in RCy3::CytoscapeWindowFromNetwork():\n\t no network named '%s' exists in Cytoscape >> choose from the following titles: ", title), stderr())
			write(as.character(getNetworkList(obj)), stderr())
            return(NA)
        }
		
		# if title=NA, fill in value from current network retrieved above
		if(is.na(title)){
		    title = getNetworkName(existing.suid)
		    #write(sprintf("Connecting to current network named '%s'", title), stderr())
		}
		
		# create minimal Cytoscape Window
        cy.window <- new('CytoscapeWindowClass', title=title, suid=existing.suid, uri=obj@uri, api=obj@api)

        # optionally, get graph from Cytoscape
        if (return.graph) {
            # copy over graph
            g.cy <- createGraphFromNetwork(cy.window, title)
            cy.window <- setGraph(cy.window, g.cy)

            # copy over obj@node.suid.name.dict
            resource.uri <- paste(cy.window@uri, cy.window@api, "networks", as.character(cy.window@suid), sep="/")
            request.res <- GET(url=resource.uri)
            request.res <- fromJSON(rawToChar(request.res$content))

            if (length(request.res$elements$nodes) != 0){
                cy.window@node.suid.name.dict = lapply(request.res$elements$nodes, function(n) { 
                    list(name=n$data$name, SUID=n$data$SUID) })
            }
            if (length(request.res$elements$edges) != 0){
                cy.window@edge.node.suid.name.dict = lapply(request.res$elements$edges, function(e) {
                    list(name=e$data$name, SUID=e$data$SUID) })
            }
        }
        return (cy.window)
} # END CytoscapeWindowFromNetwork

# ------------------------------------------------------------------------------
# Send a window from a CWnetwork from a graph (obj@graph)
setMethod('sendNetworkFromGraph', 'CytoscapeWindowClass', 
	function(obj) {
	    g = obj@graph
		g@graphData$name <- obj@title
		graph.attributes <- g@graphData
		graph.elements = list(nodes = list(), edges = list())
		
		cygraph <- toJSON(list(data = graph.attributes, elements = graph.elements))
		resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
		request.res <- POST(url = resource.uri, body = cygraph, encode = "json")
		suid <- unname(fromJSON(rawToChar(request.res$content)))
		
		return(as.character(suid))
})

# ------------------------------------------------------------------------------
setMethod ('setGraph', 'CytoscapeWindowClass', 
           function(obj, graph) {
    # copy the graph over
    loc.obj <- obj
    if (edgemode(graph) == 'undirected'){
        graph = remove.redundancies.in.undirected.graph (graph) #AP: not sure this is needed anymore...
    }
    
    loc.obj@graph = graph
    
    eval.parent(substitute(obj <- loc.obj))
})

# ------------------------------------------------------------------------------
setMethod('sendNodesFromGraph', 'CytoscapeWindowClass', function(obj) {
    loc.obj <- obj
    # returns the nodes currently stored in the graph object
    graph.network.nodes = nodes(loc.obj@graph)
    # returns the nodes currently displayed in Cytoscape
    current.cytoscape.nodes = sapply(loc.obj@node.suid.name.dict, function(n) n$name)
    
    node.node.suid.name.dict <- (0)
    
    diff.nodes = setdiff(graph.network.nodes, current.cytoscape.nodes)
    # if new nodes need to be added
    if(length(diff.nodes) > 0) {
        net.SUID = as.character(loc.obj@suid)
        
        resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, "nodes", sep="/")
        diff.nodes.JSON = toJSON(diff.nodes)
        
        write(sprintf('sending %d node(s)', length(diff.nodes)), stderr())
        
        request.res = POST(url=resource.uri, body=diff.nodes.JSON, encode="json")
        new.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
        
        for(i in 1:length(new.node.SUIDs)) {
            loc.obj@node.suid.name.dict[[length(loc.obj@node.suid.name.dict)+1]] = new.node.SUIDs[[i]]
        }
    } else {
        write('CytoscapeWindow.sendNodesFromGraph(), no new nodes to send, returning', stderr())
        return()
    }
    
    write('sendNodes completed', stderr())
    # needed for 'pass-by-reference' R functionality 
    eval.parent(substitute(obj <- loc.obj))
    })


#------------------------------------------------------------------------------------------------------------------------
setMethod('sendEdgesFromGraph', 'CytoscapeWindowClass',
  function(obj) {
      loc.obj <- obj
      net.SUID = as.character(loc.obj@suid)
      # check that there are edges in the graph
      if(length(edgeNames(loc.obj@graph)) == 0) {
          write('NOTICE in RCy3::sendEdges():\n\t no edges in graph >> function returns', stderr())
          return()
      }
      
      write(sprintf('transforming (%d) graph edges to nodePairTable', length(edgeNames(loc.obj@graph))), stderr())
      if(loc.obj@collectTimings) {
          start.time = Sys.time()
      }
      
      if(is.classic.graph(loc.obj@graph)) {
          tbl.edges = .classicGraphToNodePairTable(loc.obj@graph)
      }
      else if(is.multiGraph(loc.obj@graph)) {
          tbl.edges = .multiGraphToNodePairTable(loc.obj@graph)
      }
      
      if (loc.obj@collectTimings){
          write (sprintf(' *** create node pair table: %f secs', difftime (Sys.time(), start.time, units='secs')), stderr ())
      }
      
      # get the list of edges to be send to Cytoscape
      in.graph.edge.names = unname(cy2.edge.names(loc.obj@graph))
      # get the list of currently existing edges (from dict)
      existing.edge.names = 
          sapply(loc.obj@edge.node.suid.name.dict, function(n) {return(n$name)})
      
      diff.edges = setdiff(in.graph.edge.names, existing.edge.names)
      # in new edges need to be send to the network
      if(length(diff.edges) > 0) {
          write (sprintf('sending %d edges', nrow(tbl.edges)), stderr())
          # source nodes vector
          source.nodes = tbl.edges$source
          # target nodes vector
          target.nodes = tbl.edges$target
          # edge types vector
          edge.type = tbl.edges$edgeType
          directed = rep(TRUE, length(source.nodes))
          
          # get the SUIDs of the source nodes for the new edges
          source.node.SUIDs = .nodeNameToNodeSUID(loc.obj, source.nodes)
          # get the SUIDs of the target nodes for the new edges
          target.node.SUIDs = .nodeNameToNodeSUID(loc.obj, target.nodes)
          
          # format the new edges data for sending to Cytoscape
          edge.tbl.records = 
              apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, edge.type), MARGIN=1,
                    FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
          edge.tbl.records.JSON = toJSON(edge.tbl.records)
          resource.uri = paste(loc.obj@uri, loc.obj@api, "networks", net.SUID, "edges", sep="/")
          request.res = POST(url=resource.uri, body=edge.tbl.records.JSON, encode="json")
          
          # request.res.edge.SUIDs contains 
          # [edge.SUID, source.node.SUID, targetn.node.SUID] for each edge
          request.res.edge.data = fromJSON(rawToChar(request.res$content))
          
          edge.names = cy2.edge.names(obj@graph)
          # ctreates matrix of the format : 
          # note: column 1 contains edge.SUIDs, and columns 3 & 4 contain node.SUIDs
          #      [,1]   [,2]                     [,3]   [,4]
          # [1,] "412"  "A (phosphorylates) B"   "413"  "404"
          # [2,] "406"  "B (synthetic lethal C"  "407"  "408"
          # [3,] "407"  "C (undefined) A"        "408"  "406"
          edge.names.tbl.records = 
              apply(unname(cbind(unname(t(sapply(request.res.edge.data, unlist))), edge.names)), 
                    MARGIN=1, 
                    FUN=function(r) {list(SUID=as.numeric(unname(r[[1]])), value=unname(r[[4]]), 
                                          source.node=as.numeric(unname(r[[2]])), 
                                          target.node=as.numeric(unname(r[[3]])))})
          # CREATES DICT ENTRIES for the new edges in the following format :
          # [edge.SUID, edge.name, source.node.SUID, target.node.SUID]
          for(i in 1:length(edge.names.tbl.records)) {
              loc.obj@edge.node.suid.name.dict[[length(loc.obj@edge.node.suid.name.dict)+1]] = 
                  list(SUID=edge.names.tbl.records[[i]]$SUID, name=edge.names.tbl.records[[i]]$value, 
                       source.node=edge.names.tbl.records[[i]]$source.node, 
                       target.node=edge.names.tbl.records[[i]]$target.node)
          }
          invisible(request.res)
      } else {
          write(sprintf("NOTICE in RCy3::sendEdges():\n\t all %d edges already exist in Cytoscape - nothing new to add >> function returns", length(in.graph.edge.names)), stderr())
          return()
      }
      # simulate 'pass-by-reference' in R
      eval.parent(substitute(obj <- loc.obj))
}) # sendEdges

# ------------------------------------------------------------------------------
# Sets node attributes from graph (obj@graph)
setMethod('sendNodeAttributesFromGraph', 'CytoscapeWindowClass', 
    function(obj, attribute.name) { 
        # it might be the case that 'obj@graph' contains nodes that do NOT exist in Cytoscape
        # the below line identifies the indices of those graph nodes, which DO exist in Cytoscape
        node.indices = which(nodes(obj@graph) %in% getAllNodes(obj))
        
        if(length(node.indices) > 0) {
            node.names = nodes(obj@graph)[node.indices]
            
            values = noa(obj@graph, attribute.name)[node.indices]
            
            caller.specified.attribute.class = 
                attr(nodeDataDefaults(obj@graph, attribute.name), 'class')
            invisible(setNodeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, node.names, values))
        } else {
            write(sprintf("WARNING in RCy3::setNodeAttributes():\n\t before setting node attributes, please first send the graph nodes to Cytoscape >> function aborted"), stderr())
        }
})
## END setNodeAttributes

# ------------------------------------------------------------------------------
setMethod('sendEdgeAttributesFromGraph', 'CytoscapeWindowClass', 
    function(obj, attribute.name) {
        cyrest.edge.names = as.character(cy2.edge.names(obj@graph))
        # user might have entered the names of edges that do NOT exist
        # the below line will return the indices of the nodes that DO exist
        edge.indices = which(cyrest.edge.names %in% getAllEdges(obj))
        
        if(length(edge.indices) > 0) {
            edge.names = cyrest.edge.names[edge.indices]
            edge.names.tilde = names(cy2.edge.names(obj@graph)[edge.indices])
            edge.names.with.bars = gsub('~', '|', edge.names.tilde)
            
            values = eda(obj@graph, attribute.name)[edge.names.with.bars]
            
            caller.specified.attribute.class = attr(edgeDataDefaults(obj@graph, attribute.name), 'class')
            
            invisible(setEdgeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, edge.names, values))
        } else {
            write(sprintf("WARNING in RCy3::setEdgeAttributes():\n\t before setting edge attributes, please first send the graph edges to Cytoscape >> function aborted"), stderr())
        }        
}) 
## END setEdgeAttributes

# ------------------------------------------------------------------------------
setMethod('displayGraph', 'CytoscapeWindowClass', function(obj) {
    # needed to simulate 'pass-by-reference' behavior in R
    loc.obj <- obj
    
    if(length(nodes(loc.obj@graph)) == 0) {
        write('RCy3::displayGraph, cannot display empty(0 nodes) graph, returning...', stderr())
        return()
    }
    
    node.count = length(nodes(loc.obj@graph))
    edge.count = length(edgeNames(loc.obj@graph))
    node.attribute.count = length(noa.names(loc.obj@graph)) * node.count
    edge.attribute.count = length(eda.names(loc.obj@graph)) * edge.count
    
    estimated.time = predictTimeToDisplayGraph(loc.obj)
    # if (execution)time measurement option is turned on, save the current time
    if (loc.obj@collectTimings) {
        method.start.time = Sys.time()
        # start time (for sending nodes to Cytoscape) 
        stepwise.start.time = Sys.time()
    }
    
    write(sprintf('estimated displayGraph time: %8.1f seconds', estimated.time), stderr())
    write(sprintf('adding %d nodes...', length(nodes(obj@graph))), stderr())
    
    sendNodesFromGraph(loc.obj)
    
    if(loc.obj@collectTimings) {
        current.step.exec.time = difftime(Sys.time(), stepwise.start.time, units='secs')
        write(sprintf(' *** sendNodes: %f secs', current.step.exec.time, stderr()))
        # start time (for sending node attributes to Cytoscape)
        stepwise.start.time = Sys.time()
    }
    
    # sends edges to Cytoscape
    write (sprintf ('adding %d edges...', length (edgeNames (loc.obj@graph))), stderr ())
    sendEdgesFromGraph (loc.obj)
    
    if (obj@collectTimings) {
        write (sprintf (' *** sendEdges: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
    }
    
    # sending node attributes
    write ('adding node attributes...', stderr ())
    
    # send node attributes from R to Cytoscape
    sapply (noa.names (loc.obj@graph), function (name) {print (name); setNodeAttributes (loc.obj, name)})
    
    if (obj@collectTimings) {
        write (sprintf (' *** send node attributes: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
    }
    
    # send edge attributes
    write ('adding edge attributes...', stderr ())
    edgeAttributeNames = eda.names (loc.obj@graph)
    sapply (eda.names (loc.obj@graph), function (name) {print (name); setEdgeAttributes (loc.obj, name)})
    
    if (obj@collectTimings) {
        write (sprintf (' *** send edge attributes: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
        actual.time = difftime (Sys.time (), method.start.time, units='secs')
        write (sprintf (' *** leaving displayGraph, predicted duration %f secs,  actual %f secs', as.integer (round (estimated.time)),
                        as.integer (round (actual.time))), stderr ())
    } # if collectTimings
    
    # pseudo R 'pass-by-reference': cw now contains the [node suid,node name] pairs
    eval.parent(substitute(obj <- loc.obj))
}) 
## END displayGraph

# ------------------------------------------------------------------------------
setMethod('predictTimeToDisplayGraph', 'CytoscapeWindowClass', 
    function(obj) {
        g = obj@graph
        node.count = length(nodes(g))
        edge.count = length(edgeNames(g))
        noa.count = length(noa.names(g)) * node.count
        eda.count = length(eda.names(g)) * edge.count
        prediction = (0.002 * node.count) + (0.010 * edge.count) + (0.001 * noa.count) + (0.001 * eda.count)
        return (prediction)
})
## END predictTimeToDisplayGraph






noa.names = function(graph)
{
  return(names(nodeDataDefaults(graph)))
} # noa.names
#------------------------------------------------------------------------------------------------------------------------
eda.names = function(graph)
{
  return(names(edgeDataDefaults(graph)))
} # eda.names
#------------------------------------------------------------------------------------------------------------------------
noa = function(graph, node.attribute.name)
{
  if(!node.attribute.name %in% noa.names(graph))
    return(NA)
  return(unlist(nodeData(graph, attr=node.attribute.name)))
} # noa
#------------------------------------------------------------------------------------------------------------------------
# return the value of every edge in the graph for the specified attribute
eda = function(graph, edge.attribute.name)
{
  if(!edge.attribute.name %in% eda.names(graph))
    return (NA)
  return(unlist(edgeData(graph, attr=edge.attribute.name)))
} # eda

# ------------------------------------------------------------------------------
# use the expected 'edgeType' attribute to create cytoscape-style 'A (edgeType) B' 
# edge names from a graphNEL
# edgeNames (g) # "A~B" "B~C" "C~A"
# if there is no edge attribute named 'edgeType', then create edges(uninterestingly) named 'A (edge) B'
cy2.edge.names = function(graph, R.edge.names=NA)
{
   #printf('running new version of cy2.edge.names')
   if(length(edges(graph)) == 0) {
      return(NA)
   }
   
   edgeType.attribute.present = TRUE
   edge.type = 'unspecified'
   if('edgeType' %in% names(edgeDataDefaults(graph))) {
       # vector containing the 'edgeType'-attribute value for every edge
      edge.type = as.character(eda(graph, 'edgeType'))
   }
   
   tokens = strsplit(.rcyEdgeNames(graph), '~')
   a = sapply (tokens, function (tok) tok [1])
   b = sapply (tokens, function (tok) tok [2])
   edge.type = paste (' (', edge.type, ') ', sep='')
   edge.names = paste (a, edge.type, b, sep='')
   
   names (edge.names) = .rcyEdgeNames (graph)
   
   if (!(length (R.edge.names) == 1 && is.na (R.edge.names))) {  # we were given some subset of all edges to extract and get cy2 names for.  do that here
      new.edgeNames.tilde = gsub ('\\|', '~', R.edge.names)
      if (length (intersect (names (edge.names), new.edgeNames.tilde)) > 0){
         edge.names = edge.names [new.edgeNames.tilde]
      }
   }
   
   return (edge.names)

} # cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
getAdjacentEdgeNames = function (graph, node.names) 
{
    all.edge.names = cy2.edge.names (graph) 
    all.edge.names.cyStyle = as.character (all.edge.names) 
    indices.of.edges.with.nodes = c () 
    
    for (node in node.names) { 
        node.regex.nodeA = sprintf ('^%s ', node)
        node.regex.nodeB = sprintf (' %s$', node)
        indices.A = grep (node.regex.nodeA, all.edge.names.cyStyle) 
        indices.B = grep (node.regex.nodeB, all.edge.names.cyStyle) 
        indices.of.edges.with.nodes = c (indices.of.edges.with.nodes, indices.A, indices.B) 
    } # for node

    return (unique (as.character (all.edge.names) [indices.of.edges.with.nodes]))
    
} # getAdjacentEdgeNames
#------------------------------------------------------------------------------------------------------------------------
makeSimpleGraph = function ()
{
  g = new ('graphNEL', edgemode='directed')

  g = .initNodeAttribute (g, 'type', 'char', 'undefined')
  g = .initNodeAttribute (g, 'lfc', 'numeric', 1.0)
  g = .initNodeAttribute (g, 'label', 'char', 'default node label')
  g = .initNodeAttribute (g, 'count', 'integer', 0)

  g = .initEdgeAttribute (g, 'edgeType', 'char', 'undefined')
  g = .initEdgeAttribute (g, 'score', 'numeric', 0.0)
  g = .initEdgeAttribute (g, 'misc',   'char', 'default misc')

  g = graph::addNode ('A', g)
  g = graph::addNode ('B', g)
  g = graph::addNode ('C', g)
  nodeData (g, 'A', 'type') = 'kinase'
  nodeData (g, 'B', 'type') = 'transcription factor'
  nodeData (g, 'C', 'type') = 'glycoprotein'

  nodeData (g, 'A', 'lfc') = -3.0
  nodeData (g, 'B', 'lfc') = 0.0
  nodeData (g, 'C', 'lfc') = 3.0

  nodeData (g, 'A', 'count') = 2
  nodeData (g, 'B', 'count') = 30
  nodeData (g, 'C', 'count') = 100

  nodeData (g, 'A', 'label') = 'Gene A'
  nodeData (g, 'B', 'label') = 'Gene B'
  nodeData (g, 'C', 'label') = 'Gene C'

  g = graph::addEdge ('A', 'B', g)
  g = graph::addEdge ('B', 'C', g)
  g = graph::addEdge ('C', 'A', g)

  edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
  edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'

  edgeData (g, 'A', 'B', 'score') =  35.0
  edgeData (g, 'B', 'C', 'score') =  -12

  return (g)

} # makeSimpleGraph
#------------------------------------------------------------------------------------------------------------------------
# create, display and render the 3-node, 3-edge simple graph
demoSimpleGraph = function ()
{
    window.title = 'demo.simpleGraph'
    cy = CytoscapeConnection ()
    if (window.title %in% as.character (getNetworkList (cy)))
    deleteNetwork (cy, window.title)
    
    g.simple = makeSimpleGraph ()
    cws = CytoscapeWindow (window.title, g.simple)
    
    displayGraph (cws)
    layoutNetwork (cws, 'grid')
    setNodeLabelRule (cws, 'label')
    
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#A0AA00', '#FF0000')
    setDefaultNodeBorderWidth (cws, 5)
    setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeRule (cws, 'count', count.control.points, sizes, mode='interpolate')
    setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    
    invisible (cws)

} # demoSimpleGraph

# ------------------------------------------------------------------------------
makeRandomGraph = function(node.count=12, seed=123)
{
  set.seed(seed); 
  #if(node.count > 26) node.count = 26
  node.names = as.character(1:node.count)
  g = randomGraph(node.names, M <- 1:2, p = 0.6)
  attr(edgeDataDefaults(g, attr="weight"), "class") = "DOUBLE"
  edgeDataDefaults(g, 'pmid') = '9988778899'
  attr(edgeDataDefaults(g, attr="pmid"), "class") = "STRING"
  return(g)
} # makeRandomGraph

#------------------------------------------------------------------------------------------------------------------------
# Robert Flight offered this replacement, having encountered painfully slow execution with a 5k edge undirected graph
# this fast version, likes its slow predecessor, compensates for the (in my view) flawed implementation of undirected
# graphNELs by converting them to directed graphs.
# but because undirected graphs are logically sound, and representationally useful, this is only a temporary fix.
# a redesign of this aspect of the graphNEL class is needed.
#
# original comments:
# the bioconductor graph class stores undirected graph edge attributes redundantly.  bioc's nishant says (email, 2 sep 2010):
#
# The people who started the graph package decided to return duplicate edge attributes / weights for the undirected
# case. ie if you have an edge a-b and the graph is undirected, methods such as edgeWeights, edgeData etc will end up
# returning duplicate values for the attribute for a-b and b-a.  That was a design decision taken by the creators of the
# package and I do not think it will be possible to change that now.  I guess the solution might be to create your own
# edgeWeights and edgeData methods in your package that retrieve only the non-duplicated attributes for the undirected
# case.
#
remove.redundancies.in.undirected.graph = function(gu) 
{
  if (length(nodes(gu)) == 0) 
      return(new("graphNEL", edgemode = "directed"))

  g <- new("graphNEL", edgemode = "directed")

  if (length(edgeDataDefaults(gu)) > 0) 
      edgeDataDefaults(g) <- edgeDataDefaults(gu)

  if (length(nodeDataDefaults(gu)) > 0) 
      nodeDataDefaults(g) <- nodeDataDefaults(gu)

  g <- addNode(nodes(gu), g)

  allNodes <- nodes(gu)

  noa.name <- invisible(lapply(noa.names(gu), function(noa.name) {
      nodeData(g, allNodes, noa.name) <- nodeData(gu, allNodes, noa.name)
  }))

  if (length(edgeNames(gu)) == 0) 
      return(g)

  edge.names <- edgeNames(gu)
  edge.node.pairs <- strsplit(edge.names, "\\~")
  source.nodes <- sapply(edge.node.pairs, function(x) x[1])
  target.nodes <- sapply(edge.node.pairs, function(x) x[2])

  g = graph::addEdge(source.nodes, target.nodes, g)

  invisible(lapply(eda.names(gu), function(eda.name) {
      edgeData(g, source.nodes, target.nodes, eda.name) <- edgeData(gu, source.nodes, 
          target.nodes, eda.name)
  }))

  return(g)

}  # remove.redundancies.in.undirected.graph

#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been updated with new ones from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new nodes.
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new nodes, use the standard method 'setNodeAttributesDirect' to send them to cytoscape
# 
.sendNodeAttributesForGraph = function(obj, other.graph, attribute.name, new.node.indices)
{
    caller.specified.attribute.class = attr(nodeDataDefaults(other.graph, attribute.name), 'class')
    if(is.null(caller.specified.attribute.class)) {
        msg1 = sprintf('Error! RCytoscape:::.sendNodeAttributesForGraph. You must initialize the "%s" node attribute.', attribute.name)
        msg2 = sprintf('        example: my.graph = .initNodeAttribute(my.graph, attr="moleculeType", "char", "unspecified")')
        write(msg1, stderr())
        write(msg2, stderr())
        return(NA)
    }
    # only add attributes for new nodes, unique to the new graph 'other.graph'
    # new.node.names = setdiff(nodes(other.graph), nodes(obj@graph))
    new.node.names = nodes(other.graph)[new.node.indices]
    values = noa(other.graph, attribute.name)[new.node.names]
    invisible(setNodeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, new.node.names, values))
} # END .sendNodeAttributesForGraph 

#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been extended with all the new nodes and edges from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new edges
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new edges, use the standard method 'setEdgeAttributesDirect' to send them to cytoscape
# oddities: edge naming is a tricky business.  cytoscape lablels edges like this:
#    <sourceNode> (interactionType) <target.node.name>
# RCy3 provide a utility function for retrieving them from an R graph object,   cy2.edge.names (g)
# which uses the edgeNames (g) method to get the R names
# edgeNames (g2)  # [1] "A~E" "A~B" "D~E"
# thus, R has a little inconsistency:  sometimes using the tilda, sometimes the vertical bar
#                 A~E                 A~B                 D~E 
#     "A (inferred) E" "A (unspecified) B"  "D (literature) E" 
# names (edgeData (g2, attr='edgeType'))
#    [1] "A|E" "A|B" "D|E"
# for historical reasons, and maybe laziness, these two conventions are supported here, at the cost of calling gsub on the edge
# names, so that A~E becomes A|E, setting the stage for calling 
#   values = eda (g, attribute.name) [new.edge.names.with.bar.delimitor]
# below, and thereby ensuring that only the attributes of new edges are sent to Cytoscape

.sendEdgeAttributesForGraph = function (obj, other.graph, attribute.name, new.edge.indices)
{
    caller.specified.attribute.class = attr(edgeDataDefaults(other.graph, attribute.name), 'class')
    
    if(is.null(caller.specified.attribute.class)) {
        msg1 = sprintf('Error!  RCytoscape:::.sendEdgeAttributesForGraph. You must initialize the "%s" edge attribute.', attribute.name)
        msg2 = sprintf('        example:  my.graph = .initEdgeAttribute (my.graph, attr="edgeType", "char", "unspecified")')
        write(msg1, stderr())
        write(msg2, stderr())
        return(NA)
    }
    
    # send only attributes for edges which are unique to other.graph; 
    # we assume that any existing edges already have their attributes
    new.edge.names = unname(cy2.edge.names(other.graph)[new.edge.indices])
    
    if(length(new.edge.names) == 0) {
        return()
    }
    
    values = eda(other.graph, attribute.name)[new.edge.indices]
    invisible(setEdgeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, new.edge.names, values))
} # .sendEdgeAttributesForGraph 



# cyPlot
# New RCy3 function to read node and edge attributes according to class()
#
# Given a node attribute data frame (node.df) with the node names in column 1, 
# and an edge attribute data.frame (edge.df) with node names in the first two columns,
# cyPlot creates a graphNEL object with nodes, edges, and their attributes 
# that can be loaded into Cytoscape with CytoscapeWindow. 
#
#  Author: Mark Grimes
#	[cyPlot.5 in MGRCyFunctions.R]
#########################################################################################
#
cyPlot <- function (node.df, edge.df) {
  edge.nodes <- unique(c(as.character(edge.df[,1]),
                         as.character(edge.df[,2])))		
  mydata <- new("graphNEL",
                edgemode = 'directed',
                nodes = unique(c(as.character(node.df[, 1]),
                                 edge.nodes)))
  #	Set up and load all the node attributes
  # read class and convert factor to character as required
  node.df[,1] <- as.character(node.df[,1])
  edge.df[,1:2] <- sapply(edge.df[,1:2],
                          as.character)
  node.class <- sapply (node.df,
                        class)
  if (any(grep("factor", node.class))) {
    node.df[, grep("factor", node.class)] <- sapply(node.df[, grep("factor", node.class)],
                                                    as.character) }
  
  if (any(grep("integer", node.class))) {
    node.df[, grep("integer", node.class)] <- sapply(node.df[, grep("integer", node.class)],
                                                     as.numeric) }
  
  node.class <- sapply(node.df,
                       class)
  edge.class <- sapply(edge.df,
                       class)
  if (any(grep("factor", edge.class))) {
    edge.df[, grep("factor", edge.class)] <- sapply(edge.df[, grep("factor", edge.class)],
                                                    as.character) }
  edge.class <- sapply(edge.df,
                       class)
  
  # Nodes and attributes
  if (length(grep("character", node.class)) > 1) {
    for (i in 2:length(grep("character", node.class))) {
      mydata <- .initNodeAttribute(graph = mydata,
                                  attribute.name = names(node.class[grep("character", node.class)])[i],
                                  attribute.type = 'char',
                                  default.value = 'undefined') 
      nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("character", node.class)])[i]) <- as.character(node.df[,grep("character", node.class)[i]])		}
  }
  
  if (length(grep("numeric", node.class))){ 
    for (i in 1:length(grep("numeric", node.class))) {	
      mydata <- .initNodeAttribute(graph = mydata,
                                  attribute.name = names(node.class[grep("numeric", node.class)])[i],
                                  attribute.type = 'numeric',
                                  default.value = 0.0) 
      nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("numeric", node.class)])[i]) <- as.numeric(node.df[,grep("numeric", node.class)[i]])	}	
  }
  
  # Edges and attributes
  mydata = addEdge(as.vector(edge.df[,1],
                             mode = "character"),
                   as.vector(edge.df[,2],
                             mode = "character"),
                   mydata)
  
  if (length(grep("character", edge.class)) > 2){
    for (i in 3:length(grep("character", edge.class))) {
      mydata <- .initEdgeAttribute(graph = mydata,
                                  attribute.name = names(edge.df[,grep("character", edge.class)])[i],
                                  attribute.type = 'char',
                                  default.value = 'undefined')
      edgeData(mydata, as.vector(edge.df[,1], mode = "character"), as.vector(edge.df[,2], mode = "character"), attr = names(edge.df[,grep("character", edge.class)])[i]) <- as.character(edge.df[,grep("character", edge.class)[i]])		}
  }
  if (any(grep("numeric", edge.class))){
    for (i in 1:length(grep("numeric", edge.class))) {	
      mydata <- .initEdgeAttribute(mydata,
                                  attribute.name = names(edge.class[grep("numeric", edge.class)])[i],
                                  attribute.type = "numeric",
                                  default.value = 0)
      edgeData(mydata, as.vector(edge.df[,1], mode = "character"), as.vector(edge.df[,2], mode = "character"), attr = names(edge.class[grep("numeric", edge.class)])[i]) <- as.numeric(edge.df[,grep("numeric", edge.class)[i]])	}	
  }
  return(mydata)
}
# END cyPlot




