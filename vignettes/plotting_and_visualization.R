############################################################################################
############################################################################################
# Plotting networks in R - an example how to plot a network and 
# customize its appearance in Cytoscape directly from R using 
# the RCy3 package
#
# From Vessy's "Fun with R blog": http://www.vesnam.com/Rblog/viznets5/
############################################################################################
############################################################################################
# Clear workspace 
# rm(list = ls())
############################################################################################

# Read a data set. 
# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction
dataSet <- read.table("./data/lesmis.txt", header = FALSE, sep = "\t")

# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(dataSet, directed=FALSE))

# Print number of nodes and edges
# igraph::vcount(gD)
# igraph::ecount(gD)

############################################################################################
# Calculate some node properties and node similarities that will be used to illustrate 
# different plotting abilities

# Calculate degree for all nodes
degAll <- igraph::degree(gD, v = igraph::V(gD), mode = "all")

# Calculate betweenness for all nodes
betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
rm(betAll)

#Calculate Dice similarities between all pairs of nodes
dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")

############################################################################################
# Add new node and edge attributes based on the calculated node properties/similarities

gD <- igraph::set.vertex.attribute(gD, "degree", index = igraph::V(gD), value = degAll)
gD <- igraph::set.vertex.attribute(gD, "betweenness", index = igraph::V(gD), value = betAll.norm)

# Check the attributes
# summary(gD)

F1 <- function(x) {data.frame(V4 = dsAll[which(igraph::V(gD)$name == as.character(x$V1)), which(igraph::V(gD)$name == as.character(x$V2))])}
dataSet.ext <- plyr::ddply(dataSet, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))

gD <- igraph::set.edge.attribute(gD, "weight", index = igraph::E(gD), value = 0)
gD <- igraph::set.edge.attribute(gD, "similarity", index = igraph::E(gD), value = 0)

# The order of interactions in dataSet.ext is not the same as it is in dataSet or as it is in the edge list
# and for that reason these values cannot be assigned directly

for (i in 1:nrow(dataSet.ext))
{
    igraph::E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$weight <- as.numeric(dataSet.ext$V3)
    igraph::E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$similarity <- as.numeric(dataSet.ext$V4)
}

# Check the attributes
# summary(gD)

rm(dataSet,dsAll, i, F1)

############################################################################################
# Now, let's do Cytoscape plots

# Update: You can go straight from igraph to Cytoscape, sending all attributes and displaying graph
gDCW<-RCy3::createNetworkFromIgraph(gD,new.title='Les Miserables',return.graph=TRUE)

# # First, we need to transform our network from the igraph to graphnel format
# gD.cyt <- igraph::as_graphnel(gD)
# 
# # Check if attributes have been passed
# # graph::nodeData(gD.cyt, igraph::V(gD)$name, 'degree')
# # graph::nodeData(gD.cyt, igraph::V(gD)$name, 'betweenness')
# # graph::edgeData(gD.cyt, as.character(dataSet.ext$V1), as.character(dataSet.ext$V2), 'weight')
# # graph::edgeData(gD.cyt, as.character(dataSet.ext$V1), as.character(dataSet.ext$V2), 'similarity')
# 
# # We have to create attributes for graphNEL
# # We'll keep the same names as before
# # In RCytoscape, this would ensure that the values of attributes are passed directly from igraph.
# # However, this does not work with RCy3 right now (not sure if it is a bug or a feature has changed).
# # Thus, we need to do send attributes to Cytoscape
# 
# gD.cyt <- RCy3::initNodeAttribute(gD.cyt, 'degree', 'numeric', 0) 
# gD.cyt <- RCy3::initNodeAttribute(gD.cyt, 'betweenness', 'numeric', 0) 
# gD.cyt <- RCy3::initEdgeAttribute (gD.cyt, "weight", 'integer', 0)
# gD.cyt <- RCy3::initEdgeAttribute (gD.cyt, "similarity", 'numeric', 0)
# 
# # Next, we will create a new graph window in cytoscape
# gDCW <- RCy3::CytoscapeWindow("Les Miserables", graph = gD.cyt, overwriteWindow = TRUE)
# 
# # We can display graph, with defaults color/size scheme
# RCy3::displayGraph(gDCW)
# 
# # Now let's send/load node and edge attributes into Cytoscape
# 
# ##########
# # This should theoretically work, but there are some problems with attributes when networks
# # are created from data frames (see https://github.com/tmuetze/Bioconductor_RCy3_the_new_RCytoscape/issues/25)
# # I'll keep this code uncommented, but right now, it doesn't do anything
# 
# # setNodeAttributes should transfer the specified node attributes, for all nodes, the named node attribute 
# # from the R graph (found in obj@graph) to Cytoscape. 
# attribute.names <- RCy3::noa.names(gDCW@graph)
# 
# # Print list of attribute names to see if they are ok
# # attribute.names 
# 
# # All nodes should already be in
# RCy3::sendNodes(gDCW)
# 
# for (attribute.name in attribute.names){
#     RCy3::setNodeAttributes(gDCW, attribute.name)
# }
# 
# attribute.names <- RCy3::eda.names(gDCW@graph)
# 
# # All edges should already be in
# RCy3::sendEdges(gDCW)
# 
# for (attribute.name in attribute.names){
#     RCy3::setEdgeAttributes(gDCW, attribute.name)
# }
# 
# RCy3::displayGraph(gDCW)
# 
# ##########
# # The alternative, when we set attributes directly, works fine,
# # so we will use it now (although, it seems kind of repetative)
# 
# RCy3::setNodeAttributesDirect(gDCW, 'degree', 'numeric', igraph::V(gD)$name, igraph::V(gD)$degree)
# RCy3::setNodeAttributesDirect(gDCW, 'betweenness', 'numeric', igraph::V(gD)$name, igraph::V(gD)$betweenness)
# RCy3::setEdgeAttributesDirect(gDCW, 'weight', 'integer', as.character (RCy3::cy2.edge.names (gDCW@graph)), graph::edgeData(gD.cyt, as.character(dataSet.ext$V1), as.character(dataSet.ext$V2), 'weight'))
# RCy3::setEdgeAttributesDirect(gDCW, 'similarity', 'numeric', as.character (RCy3::cy2.edge.names (gDCW@graph)), graph::edgeData(gD.cyt, as.character(dataSet.ext$V1), as.character(dataSet.ext$V2), 'similarity'))

##########
# Now let's decide on a layout

# If you also want to choose a layout from R, a list  of available layouts can be accessed as follow:
#cy <- RCy3::CytoscapeConnection() #this is now optional
hlp <-RCy3::getLayoutNames()

# We'll select the "fruchterman-rheingold" layout. This layout is the layout number 10 
# To see properties for the given layout, use:
# RCy3::getLayoutPropertyNames(cy, hlp[10]) #not always [10]; better to use name
# We can choose any property we want and provide them as a list
RCy3::setLayoutProperties (gDCW, "fruchterman-rheingold", list (gravity_multiplier = 'similarity', nIterations = 1000))
RCy3::layoutNetwork(gDCW, "fruchterman-rheingold")

# But that is a crazy layout, so let's try "force-directed" instead
RCy3::layoutNetwork(gDCW, "force-directed")

##########
# Finally, we can define rules for nodes:
RCy3::setNodeColorRule(gDCW, 'degree', c(min(degAll), mean(degAll), max(degAll)), c('#F5DEB3', '#FFA500', '#FF7F50', '#FF4500', '#FF0000'), mode = 'interpolate')
RCy3::setNodeSizeRule(gDCW, 'betweenness', c(min(betAll.norm), mean(betAll.norm), max(betAll.norm)), c(30, 45, 60, 80, 100), mode = 'interpolate')

# And edges:
RCy3::setEdgeLineWidthRule(gDCW, 'weight', c(min(as.numeric(dataSet.ext$V3)), mean(as.numeric(dataSet.ext$V3)), max(as.numeric(dataSet.ext$V3))), c(1,2,3,4,5), mode='interpolate')
RCy3::setEdgeColorRule(gDCW, 'weight', c(min(as.numeric(dataSet.ext$V3)), mean(as.numeric(dataSet.ext$V3)), max(as.numeric(dataSet.ext$V3))), c('#FFFF00', '#00FFFF', '#00FF7F', '#228B22', '#006400'), mode='interpolate')

# If you don't see your changes (e.g., if all edges disappear), then check for warnings in the Style Panel in Cytoscape.
# Faulty mappings can cause elements to not display correctly.

# Also, it may simply be due to graphics rendering threshold. You can override it with this:
RCy3::showGraphicsDetails()

# We will define our own default color/size schema after we defined node and edge rules, due to
# possible issues when using rules
RCy3::setDefaultBackgroundColor(gDCW, '#D3D3D3')
RCy3::setDefaultNodeBorderColor(gDCW, '#000000')
RCy3::setDefaultNodeBorderWidth(gDCW, 3)
RCy3::setDefaultNodeShape(gDCW, 'ellipse')
RCy3::setDefaultNodeFontSize(gDCW, 20)
RCy3::setDefaultNodeLabelColor(gDCW, '#000000')

############################################################################################

sessionInfo()


# R version 3.3.1 (2016-06-21)
# Platform: x86_64-redhat-linux-gnu (64-bit)
# Running under: Fedora 23 (Workstation Edition)
#
# locale:
# [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
# [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
# [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
# [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods  
# [7] base     
# 
# loaded via a namespace (and not attached):
# [1] httr_1.2.1          R6_2.1.2            plyr_1.8.4         
# [4] magrittr_1.5        parallel_3.3.1      tools_3.3.1        
# [7] igraph_1.0.1        RCurl_1.95-4.8      curl_1.1           
# [10] Rcpp_0.12.6         RJSONIO_1.3-0       BiocGenerics_0.18.0
# [13] RCy3_1.2.0          bitops_1.0-6        stats4_3.3.1       
# [16] graph_1.50.0 
#
#####
# Cytoscape version: 3.4.0
# Java version: 1.8.0_101
# cyREST version: 3.3.4

############################################################################################