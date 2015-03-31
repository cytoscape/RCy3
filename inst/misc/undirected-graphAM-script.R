library(graph)
library(RCytoscape)

# create the graph
adMatrix = rbind(
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10),
	rep(1, 10)
)
colnames(adMatrix) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
rownames (adMatrix) = colnames (adMatrix)

#adMatrix = rbind (rep (1, 3), rep (1,3), rep (1,3))
adMatrix = rbind (rep (1, 10), rep (1,10), rep (1,10), rep (1,10), rep (1,10), rep (1,10), rep (1,10), rep (1,10), rep (1,10), rep (1,10))
#colnames (adMatrix) = c ('A', 'B', 'C')
#rownames (adMatrix) = colnames (adMatrix)


g = new("graphAM", adjMat = adMatrix, edgemode="undirected")

# prep edge data
nodeList = nodes(g)
nodeListLength = length(nodeList)
from = c()
to  = c()
dat = c()
for (i in seq(1, nodeListLength)) {
	node = nodeList[i]
	for (j in seq(i, nodeListLength)) {
		node2 = nodeList[j]
		from = c(from, node)
		to = c(to, node2)
		dat = c(dat, i + j)
	}
}

destroyAllWindows (cy)
# add in the edge data
g = initEdgeAttribute(g, "att", "numeric", 0)
edgeData(g, from, to, "att") = as.integer(dat)
edgeData(g, to, from, "att") = as.integer(dat)

# show in Cytoscape
# the graph is added in a second step since adding it the recommended way (via 
#  the CytoscapeWindow constructor) cleans things up properly
g2 = RCytoscape:::remove.redundancies.in.undirected.graph (g)

cw = new.CytoscapeWindow("graphAMTest")
cw@graph = g2
displayGraph(cw)
layout(cw, "jgraph-circle")
redraw(cw)
