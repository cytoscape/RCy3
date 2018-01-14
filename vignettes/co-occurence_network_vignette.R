## ---- message=FALSE------------------------------------------------------
library(RCy3)
library(igraph)
library(RColorBrewer)

## ------------------------------------------------------------------------
cy <- CytoscapeConnection()
deleteAllWindows(cy)

## ----read-in-data--------------------------------------------------------
## scripts for processing located in "inst/data-raw/"
prok_vir_cor <- read.delim("./data/virus_prok_cor_abundant.tsv")

## ------------------------------------------------------------------------
graph_vir_prok <- igraph::simplify(igraph::graph.data.frame(prok_vir_cor,
                                            directed = FALSE))

## ------------------------------------------------------------------------
phage_id_affiliation <- read.delim("./data/phage_ids_with_affiliation.tsv")
bac_id_affi <- read.delim("./data/prok_tax_from_silva.tsv")

## ------------------------------------------------------------------------
genenet.nodes <- as.data.frame(igraph::vertex.attributes(graph_vir_prok))

## not all have classification, so create empty columns
genenet.nodes$phage_aff <- rep("not_class", nrow(genenet.nodes))
genenet.nodes$Tax_order <- rep("not_class", nrow(genenet.nodes))
genenet.nodes$Tax_subfamily <- rep("not_class", nrow(genenet.nodes))

for (row in seq_along(1:nrow(genenet.nodes))){
  if (genenet.nodes$name[row] %in% phage_id_affiliation$first_sheet.Phage_id_network){
    id_name <- as.character(genenet.nodes$name[row])
    aff_to_add <- unique(subset(phage_id_affiliation,
                                first_sheet.Phage_id_network == id_name,
                                select = c(phage_affiliation,
                                           Tax_order,
                                           Tax_subfamily)))
    genenet.nodes$phage_aff[row] <- as.character(aff_to_add$phage_affiliation)
    genenet.nodes$Tax_order[row] <- as.character(aff_to_add$Tax_order)
    genenet.nodes$Tax_subfamily[row] <- as.character(aff_to_add$Tax_subfamily)
  }
}

## ------------------------------------------------------------------------
## do the same for proks
genenet.nodes$prok_king <- rep("not_class", nrow(genenet.nodes))
genenet.nodes$prok_tax_phylum <- rep("not_class", nrow(genenet.nodes))
genenet.nodes$prok_tax_class <- rep("not_class", nrow(genenet.nodes))

for (row in seq_along(1:nrow(genenet.nodes))){
  if (genenet.nodes$name[row] %in% bac_id_affi$Accession_ID){
    aff_to_add <- unique(subset(bac_id_affi,
                                Accession_ID == as.character(genenet.nodes$name[row]),
                                select = c(Kingdom,
                                           Phylum,
                                           Class)))
    
    genenet.nodes$prok_king[row] <- as.character(aff_to_add$Kingdom)
    genenet.nodes$prok_tax_phylum[row] <- as.character(aff_to_add$Phylum)
    genenet.nodes$prok_tax_class[row] <- as.character(aff_to_add$Class)
  }
}

## ------------------------------------------------------------------------
genenet.edges <- data.frame(igraph::as_edgelist(graph_vir_prok))
names(genenet.edges) <- c("name.1",
                          "name.2")
genenet.edges$Weight <- igraph::edge_attr(graph_vir_prok)[[1]]

genenet.edges$name.1 <- as.character(genenet.edges$name.1)
genenet.edges$name.2 <- as.character(genenet.edges$name.2)
genenet.nodes$name <- as.character(genenet.nodes$name)

ug <- cyPlot(genenet.nodes,genenet.edges)

## ---- message=FALSE------------------------------------------------------
cw <- CytoscapeWindow("Tara oceans",
                      graph = ug,
                      overwriteWindow = TRUE)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
layoutNetwork(cw)
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
saveImage(cw,
          "co-occur0",
          "png",
          h = 1200)
knitr::include_graphics("./co-occur0.png")

## ------------------------------------------------------------------------
families_to_colour <- unique(genenet.nodes$prok_tax_phylum)
families_to_colour <- families_to_colour[!families_to_colour %in% "not_class"]
node.colour <- RColorBrewer::brewer.pal(length(families_to_colour),
                          "Set3")

## ------------------------------------------------------------------------
setNodeColorRule(cw,
                 "prok_tax_phylum",
                 families_to_colour,
                 node.colour,
                 "lookup",
                 default.color = "#ffffff")

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
layoutNetwork(cw)
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
saveImage(cw,
          "co-occur0_1",
          "png",
          h = 1200)
knitr::include_graphics("./co-occur0_1.png")

## ------------------------------------------------------------------------
shapes_for_nodes <- c("DIAMOND")

phage_names <- grep("ph_",
                    genenet.nodes$name,
                    value = TRUE)
setNodeShapeRule(cw,
                 "label",
                 phage_names,
                 shapes_for_nodes)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
saveImage(cw,
          "co-occur1",
          "png",
          h = 1200)
knitr::include_graphics("./co-occur1.png")

## ------------------------------------------------------------------------
setDefaultNodeBorderWidth(cw, 5)
families_to_colour <- c(" Podoviridae",
                        " Siphoviridae",
                        " Myoviridae")
node.colour <- RColorBrewer::brewer.pal(length(families_to_colour),
                          "Dark2")
setNodeBorderColorRule(cw,
                       "Tax_subfamily",
                       families_to_colour,
                       node.colour,
                       "lookup", 
                       default.color = "#000000")

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw)
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------
saveImage(cw,
          "co-occur2",
          "png",
          h = 1200)
knitr::include_graphics("./co-occur2.png")

## ------------------------------------------------------------------------
getLayoutNames(cw)

# Note: allegro-spring-electric layout is no longer available
getLayoutPropertyNames(cw, layout.name = "force-directed")
getLayoutPropertyValue(cw, "force-directed","defaultSpringCoefficient") 
getLayoutPropertyValue(cw, "force-directed","defaultSpringLength")  
getLayoutPropertyValue(cw, "force-directed","defaultNodeMass")

## ------------------------------------------------------------------------
setLayoutProperties(cw,
                    layout.name = "force-directed",
                    list(defaultSpringCoefficient = 0.00006,
                         defaultSpringLength = 80))
layoutNetwork(cw,
              layout.name = "force-directed")
fitContent(cw)

## ---- echo=FALSE---------------------------------------------------------

saveImage(cw,
          "co-occur3",
          "png", 
          h = 1200)
knitr::include_graphics("./co-occur3.png")

## ------------------------------------------------------------------------
## initiate a new node attribute
ug2 <- initNodeAttribute(graph = ug,
                          "degree",
                          "numeric",
                          0.0) 

## degree from graph package for undirected graphs not working well,
## so instead using igraph to calculate this from the original graph
nodeData(ug2, nodes(ug2), "degree") <- igraph::degree(graph_vir_prok)

cw2 <- CytoscapeWindow("Tara oceans with degree",
                      graph = ug2,
                      overwriteWindow = TRUE)

## ---- message=FALSE, results="hide"--------------------------------------
displayGraph(cw2)
layoutNetwork(cw2)

## ------------------------------------------------------------------------
degree_control_points <- c(min(igraph::degree(graph_vir_prok)),
                           mean(igraph::degree(graph_vir_prok)),
                           max(igraph::degree(graph_vir_prok)))
node_sizes <- c(20,
                20,
                80,
                100,
                110) # number of control points in interpolation mode,
                     # the first and the last are for sizes "below" and "above" the attribute seen.

setNodeSizeRule(cw2,
                "degree",
                degree_control_points,
                node_sizes,
                mode = "interpolate")

layoutNetwork(cw2,
              "force-directed")

## ---- echo=FALSE---------------------------------------------------------
fitContent(cw2)
Sys.sleep(10) # to make sure content is fit before taking an image
saveImage(cw2,
          "co-occur_degree",
          "png", h=1200)

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("./co-occur_degree.png")

## ------------------------------------------------------------------------
selectNodes(cw2,
            "GQ377772") # selects specific nodes
getSelectedNodes(cw2)

selectFirstNeighborsOfSelectedNodes(cw2)
getSelectedNodes(cw2)

## ------------------------------------------------------------------------
selectFirstNeighborsOfSelectedNodes(cw2)
getSelectedNodes(cw2)

## ------------------------------------------------------------------------
newnet <- createWindowFromSelection(cw2,
                                    "subnet",
                                    "TRUE")
layoutNetwork(newnet, "force-directed")

## ---- echo=FALSE---------------------------------------------------------
fitContent(newnet)
Sys.sleep(10)
saveImage(newnet,
          "co-occur_subnet",
          "png",
          h = 1200)

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("./co-occur_subnet.png")

