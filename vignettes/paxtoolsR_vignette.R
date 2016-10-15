## ---- message = FALSE----------------------------------------------------
library(paxtoolsr)
library(RCy3)
library(igraph)
library(RColorBrewer)

## ------------------------------------------------------------------------
sif <- paxtoolsr::toSif(system.file("extdata",
                         "biopax3-short-metabolic-pathway.owl",
                         package = "paxtoolsr"))

## ------------------------------------------------------------------------
g <- igraph::graph.edgelist(as.matrix(sif[, c(1, 3)]),
                    directed = FALSE)

g.nodes <- as.data.frame(igraph::vertex.attributes(g))
g.edges <- data.frame(igraph::as_edgelist(g))
names(g.edges) <- c("name.1",
                    "name.2")

ug <- cyPlot(g.nodes,
             g.edges)

## ---- message=FALSE------------------------------------------------------
cy <- CytoscapeConnection()
deleteAllWindows(cy)

## ---- message=FALSE------------------------------------------------------
cw <- CytoscapeWindow("Metabolic pathway from paxtoolsr",
                      graph = ug,
                      overwriteWindow = TRUE)
setDefaultNodeFontSize(cw,
                       7)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
layoutNetwork(cw,
              "force-directed")
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
Sys.sleep(10)
saveImage(cw,
          "paxtools_met_path_1",
          "png",
          h = 700)
knitr::include_graphics("./paxtools_met_path_1.png")

## ------------------------------------------------------------------------
gene <- "BDNF"
t1 <- paxtoolsr::graphPc(source = gene,
              kind = "neighborhood",
              format = "BINARY_SIF",
              verbose = TRUE)

## ------------------------------------------------------------------------
t2 <- t1[which(t1[, 2] == "controls-state-change-of"), ]

## ------------------------------------------------------------------------
ids <- unique(c(t2$PARTICIPANT_A,
                t2$PARTICIPANT_B))
t3 <- paxtoolsr::filterSif(t2,
                ids = sample(ids,
                             25))

## ------------------------------------------------------------------------
g <- igraph::graph.edgelist(as.matrix(t3[, c(1, 3)]),
                    directed = FALSE)

## ------------------------------------------------------------------------
g.nodes <- as.data.frame(igraph::vertex.attributes(g))
g.edges <- data.frame(igraph::as_edgelist(g))
names(g.edges) <- c("name.1",
                    "name.2")

ug <- cyPlot(g.nodes,
             g.edges)

## ------------------------------------------------------------------------
setDefaultNodeFontSize(cw,
                       12)

## ---- message=FALSE------------------------------------------------------
cw <- CytoscapeWindow("Pathway Commons graph query from paxtoolsr",
                      graph = ug,
                      overwriteWindow = TRUE)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
# setLayoutProperties(cw,
#                     layout.name = "allegro-spring-electric",
#                     list(gravity = 100,
#                          scale = 6))
layoutNetwork(cw,
              layout.name = "force-directed")
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
Sys.sleep(10)
saveImage(cw,
          "pathway_commons_gq",
          "png",
          h = 700)
knitr::include_graphics("./pathway_commons_gq.png")

## ------------------------------------------------------------------------
genes <- c("AKT1",
           "IRS1",
           "MTOR",
           "IGF1R")
t1 <- paxtoolsr::graphPc(source = genes,
              kind = "PATHSBETWEEN",
              format = "BINARY_SIF", 
              verbose = TRUE)

## ------------------------------------------------------------------------
t2 <- t1[which(t1[, 2] == "controls-state-change-of"),]

## ------------------------------------------------------------------------
g <- igraph::graph.edgelist(as.matrix(t2[, c(1, 3)]),
                    directed = FALSE)

## ------------------------------------------------------------------------
g.nodes <- as.data.frame(igraph::vertex.attributes(g))
g.edges <- data.frame(igraph::as_edgelist(g))
names(g.edges) <- c("name.1",
                    "name.2")

ug <- cyPlot(g.nodes,
             g.edges)

## ---- message=FALSE------------------------------------------------------
cw <- CytoscapeWindow("Subnetwork of Pathway Commons graph query from paxtoolsr",
                      graph = ug,
                      overwriteWindow = TRUE)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
layoutNetwork(cw,
              layout.name = "force-directed")

## ---- echo=FALSE---------------------------------------------------------
fitContent(cw)
Sys.sleep(10)
saveImage(cw,
          "subnet_pathway_commons_gq",
          "png",
          h = 700)
knitr::include_graphics("./subnet_pathway_commons_gq.png")

## ------------------------------------------------------------------------
# Generate a color palette that goes from white to red 
# that contains 10 colours
numColors <- 10
colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(numColors)

# Generate values that could represent some experimental values
values <- runif(length(V(g)$name))

# Scale values to generate indices from the color palette
xrange <- range(values)
newrange <- c(1,
              numColors)

factor <- (newrange[2] - newrange[1])/(xrange[2] - xrange[1])
scaledValues <- newrange[1] + (values - xrange[1]) * factor
indices <- as.integer(scaledValues)

## ------------------------------------------------------------------------
g <- cw@graph   
g <- initNodeAttribute(graph = g,
                       'indices',
                       "numeric",
                       0)
nodeData(g, nodes(g), "indices") <- indices

## ---- message = FALSE----------------------------------------------------
cw <- CytoscapeWindow("Coloured network paxtoolsr",
                      graph = g,
                      overwriteWindow = TRUE)
displayGraph(cw) # cw's graph is sent to Cytoscape

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
fitContent(cw)

## ------------------------------------------------------------------------
layoutNetwork(cw,
              layout.name = "force-directed")
setNodeColorRule(cw,
                 "indices",
                 control.points = as.numeric(c(1.0:10.0)), # needs to match type of column in Cytoscape
                 colors,
                 "lookup",
                 default.color="#ffffff")


## ---- echo=FALSE---------------------------------------------------------
fitContent(cw)
Sys.sleep(10)
saveImage(cw,
          "coloured_paxtoolsr_ex",
          "png",
          h = 700)

knitr::include_graphics("./coloured_paxtoolsr_ex.png")
knitr::include_graphics("./paxtoolsR_metadata_colour_legend.png")

