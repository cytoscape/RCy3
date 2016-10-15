## ---- message = FALSE----------------------------------------------------
library(RCy3)
library(rcellminer)
library(RColorBrewer)

## ------------------------------------------------------------------------
df <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]

moaToCompounds <- getMoaToCompounds()
Moa_names <- names(moaToCompounds)

df_knownMoaDrugs <- subset(df, MOA %in% Moa_names)

## use only those with greater than 10 experiments
df_with_knownMoaDrugs <- subset(df_knownMoaDrugs, TOTAL_EXPS > 10)

## ------------------------------------------------------------------------

long_names <- df_with_knownMoaDrugs$NAME[nchar(df_with_knownMoaDrugs$NAME) > 12] 
chopped_long_names <- gsub("^(.{12})(.*)$", "\\1", long_names)
df_with_knownMoaDrugs$NAME[nchar(df_with_knownMoaDrugs$NAME) > 12 ] <- chopped_long_names

## ------------------------------------------------------------------------
# first, delete existing windows to save memory:
deleteAllWindows(CytoscapeConnection())
cy <- CytoscapeConnection()
getCommandsWithinNamespace(cy,
                           "chemviz")

## ------------------------------------------------------------------------
g <- new("graphNEL",
         nodes = df_with_knownMoaDrugs$NAME,
         edgemode = "undirected")

cw <- CytoscapeWindow("vignette_for_chemviz",
                      graph = g,
                      overwrite = TRUE)

## ---- message = FALSE----------------------------------------------------
displayGraph(cw)
layoutNetwork(cw,
              layout.name = "grid")
showGraphicsDetails(cw, new.value)

## ------------------------------------------------------------------------
g <- cw@graph   
g <- initNodeAttribute(graph = g,
                       "SMILES",
                       "char",
                       "none")
nodeData(g, nodes(g), "SMILES") <- df_with_knownMoaDrugs$SMILES

g <- initNodeAttribute(graph = g,
                       "NSC_from_df",
                       "char",
                       "none")
nodeData(g, nodes(g), "NSC_from_df") <- df_with_knownMoaDrugs$NSC

g <- initNodeAttribute(graph = g,
                       "MOA",
                       "char",
                       "none")
nodeData(g, nodes(g), "MOA") <- df_with_knownMoaDrugs$MOA

## ---- message = FALSE----------------------------------------------------
cw <- setGraph(cw,
               g)
displayGraph(cw) # cw's graph is sent to Cytoscape

## ---- echo=FALSE---------------------------------------------------------
fitContent(cw)
Sys.sleep(10)
saveImage(cw,
          "nodes_for_chemviz",
          "png",
          h = 800)
knitr::include_graphics("./nodes_for_chemviz.png")

## ------------------------------------------------------------------------
getCommandsWithinNamespace(cw, "chemviz")

## ------------------------------------------------------------------------
getCommandsWithinNamespace(cw, "chemviz/create%20similarity")

## ------------------------------------------------------------------------
properties.list <- list(createNewNetwork = TRUE,
                        network = "current",
                        nodeList = "all")

command.name <- "chemviz/create%20similarity"

chemviz_cw <- setCommandProperties(cw,
                     command.name,
                     properties.list,
                     copy.graph.to.R = FALSE)

## ---- echo=FALSE---------------------------------------------------------
fitContent(chemviz_cw)
Sys.sleep(10)
saveImage(chemviz_cw,
          "chemviz_similarity_net",
          "png",
          h =800)
knitr::include_graphics("./chemviz_similarity_net.png")

## ------------------------------------------------------------------------
new_cw <- connectToNewestCyWindow(chemviz_cw)

## ------------------------------------------------------------------------
layoutNetwork(new_cw, "force-directed")

## ---- echo=FALSE---------------------------------------------------------
fitContent(new_cw)
Sys.sleep(10)
saveImage(new_cw,
          "chemviz_similarity_net_layout_fd",
          "png",
          h=800)
knitr::include_graphics("./chemviz_similarity_net_layout_fd.png")

## ------------------------------------------------------------------------
getCommandsWithinNamespace(new_cw, "chemviz")

## ------------------------------------------------------------------------
getCommandsWithinNamespace(new_cw, "chemviz/paint%20structures")

## ---- results="hide", message = FALSE------------------------------------
properties.list <- list(nodeList = "all")

command.name <- "chemviz/paint%20structures"

setCommandProperties(new_cw,
                     command.name,
                     properties.list,
                     copy.graph.to.R = FALSE)

## ---- echo=FALSE---------------------------------------------------------
fitContent(new_cw)
Sys.sleep(10)
saveImage(new_cw,
          "chemviz_similarity_net_node_structures",
          "png", h = 800)
knitr::include_graphics("./chemviz_similarity_net_node_structures.png")

## ------------------------------------------------------------------------
MOA_classes <- unique(df_with_knownMoaDrugs$MOA)
number_of_unique_MOA <- length(MOA_classes)
colours_for_MOA_classes <- colorRampPalette(brewer.pal(12, "Set3"))(number_of_unique_MOA)

## ------------------------------------------------------------------------
setNodeColorRule(new_cw,
                 "MOA",
                 MOA_classes,
                 colours_for_MOA_classes,
                 "lookup", 
                 default.color = "#000000")

## node font looks ugly, let's turn it off for now
setDefaultNodeFontSize(new_cw,
                       0)

## ---- echo=FALSE---------------------------------------------------------
fitContent(new_cw)
Sys.sleep(10)
saveImage(new_cw,
          "chemviz_similarity_net_coloured_by_MOA",
          "png",
          h = 1000)
# removed this so that I could knit to pdf
#<img src="./chemviz_similarity_net_coloured_by_MOA.png" width="600"> <img src="./chemviz_legend.svg" width="150">
knitr::include_graphics("./chemviz_similarity_net_coloured_by_MOA.png")
knitr::include_graphics("./chemviz_legend.png")

