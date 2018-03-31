# ==============================================================================
# Functions for working with SELECTIONS of nodes and edges in networks, including
# operations that perform selection and rely on prior selection events.
#
# I. General selection functions
# II. Node selection functions
# III. Edge selection functions
# 
# ==============================================================================
# I. General selection functions
# ------------------------------------------------------------------------------
#' @title Clear Selection
#'
#' @description If any nodes are selected in the network, they will be unselected.
#' @param type 'nodes', 'edges' or 'both' (default)
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' clearSelection()
#' }
#' @export
clearSelection <-
    function(type = 'both',
             network = NULL,
             base.url = .defaultBaseUrl) {
        net.SUID <- getNetworkSuid(network)
        
        if (type %in% c('nodes', 'both')) {
            resource.uri <-
                res <- cyrestPUT(
                    paste(
                        "networks",
                        net.SUID,
                        "tables/defaultnode/columns/selected",
                        sep = "/"
                    ),
                    parameters = list(default = "false"),
                    base.url = base.url
                )
        }
        if (type %in% c('edges', 'both')) {
            res <- cyrestPUT(
                paste(
                    "networks",
                    net.SUID,
                    "tables/defaultedge/columns/selected",
                    sep = "/"
                ),
                parameters = list(default = "false"),
                base.url = base.url
            )
        }
        invisible(res)
    }

# ==============================================================================
# II. Node selection functions
# ------------------------------------------------------------------------------
#' @title Select first neighbor nodes
#'
#' @description Select nodes directly connected to currently selected nodes. Can
#' specify connection directionality using the direction param.
#' @param direction direction of connections to neighbors to follow, e.g., incoming, outgoing, undirected, or any (default)
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return list of suids of selected nodes, including original selection
#' @examples
#' \donttest{
#' selectFirstNeighbors()
#' selectFirstNeighors('outgoing')
#' selectFirstNeighors('incoming')
#' }
#' @export
selectFirstNeighbors <-
    function(direction = 'any',
             network = NULL,
             base.url = .defaultBaseUrl) {
        suid = getNetworkSuid(network)
        cmd <-
            paste0('network select firstNeighbors="',
                   direction,
                   '" network=SUID:"',
                   suid,
                   '"')
        res <- commandsPOST(cmd, base.url = base.url)
        return(res)
    }

# ------------------------------------------------------------------------------
#' @title Select Nodes
#'
#' @description Select nodes in the network by SUID, name or other column values.
#' @param nodes List of node SUIDs, names or other column values
#' @param by.col Node table column to lookup up provide node values. Default is 'SUID'.
#' @param preserve.current.selection \code{boolean} Whether to maintain previously selected nodes.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of newly selected node SUIDs
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' selectNodes()
#' }
#' @export
selectNodes <-
    function(nodes,
             by.col = 'SUID',
             preserve.current.selection = TRUE,
             network = NULL,
             base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        
        if (!preserve.current.selection)
            clearSelection(type = 'nodes', suid, base.url)
        
        node.list.str = NULL
        for (n in nodes) {
            if (is.null(node.list.str))
                node.list.str = paste(by.col, n, sep = ":")
            else
                node.list.str = paste(node.list.str, paste(by.col, n, sep =
                                                               ":"), sep = ",")
        }
        res <-
            commandsPOST(
                paste0(
                    'network select network=SUID:"',
                    suid,
                    '" nodeList="',
                    node.list.str
                ),
                base.url = base.url
            )
        return(res)
    }

# ------------------------------------------------------------------------------
#' Select all nodes
#'
#' @description Selects all nodes in a Cytoscape Network
#' @param network name or suid of the network into which you want to select; default is "current" network
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Selects all nodes in a specified network.
#' @author Alexander Pico, Julia Gustavsen
#' @seealso \code{\link{selectNodes}}
#' @examples \donttest{
#' selectAllNodes()
#' }
#' @export
selectAllNodes <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    all_node_SUIDs <-
        cyrestGET(paste("networks", suid, "nodes", sep = "/"), base.url = base.url)
    
    # prepare selection values
    SUID.value.pairs <- lapply(all_node_SUIDs,
                               function(s) {
                                   list('SUID' = s, 'value' = TRUE)
                               })
    res <-
        cyrestPUT(
            paste(
                "networks",
                suid,
                "tables/defaultnode/columns/selected",
                sep = "/"
            ),
            body = SUID.value.pairs,
            base.url = base.url
        )
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @title Get Selected Node Count
#'
#' @description Returns the number of nodes currently selected in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{numeric}
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getSelectedNodeCount()
#' }
#' @export
getSelectedNodeCount <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        net.SUID <- getNetworkSuid(network)
        res <-
            cyrestGET(
                paste('networks', net.SUID, 'nodes', sep = "/"),
                list(column = "selected", query = "true"),
                base.url = base.url
            )
        return(length(res))
    }

# ------------------------------------------------------------------------------
#' @title Get Selected Nodes
#'
#' @description Retrieve the names of all the nodes selected in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @return \code{list} of selected node names
#' @examples \donttest{
#' getSelectedNodes()
#' }
#' @export
getSelectedNodes <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        net.SUID <- getNetworkSuid(network)
        
        if (getSelectedNodeCount(net.SUID, base.url) == 0) {
            write (sprintf ('No nodes selected.'), stdout ())
            return(NA)
        } else {
            selected.node.SUIDs <-
                cyrestGET(
                    paste("networks", net.SUID, "nodes", sep = "/"),
                    list(column = "selected", query =
                             "true"),
                    base.url = base.url
                )
            selected.node.names <-
                .nodeSUIDToNodeName(selected.node.SUIDs, net.SUID, base.url)
            return(selected.node.names)
        }
    }


# ------------------------------------------------------------------------------
#' @title Invert Node Selection
#'
#' @description Select all nodes that were not selected and deselect all nodes 
#' that were selected.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of newly selected node SUIDs
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' invertNodeSelection()
#' }
#' @export
invertNodeSelection <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        res <-
            commandsPOST(paste0('network select invert=nodes network=SUID:', suid),
                         base.url = base.url)
        return(res)
    }

# ------------------------------------------------------------------------------
#' @title Delete Selected Nodes
#'
#' @description Delete currently selected nodes from the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{named list} of deleted node suids ($nodes) as well as edge suids
#' ($edges) deleted as a result of the node deletion
#' @examples \donttest{
#' deleteSelectedNodes()
#' }
#' @export
deleteSelectedNodes <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        title = getNetworkName(network)
        commandsPOST(paste0('network delete nodeList=selected network=', title),
                     base.url = base.url)
    }

# ==============================================================================
# II. Edge selection functions
# ------------------------------------------------------------------------------
#' @title Select Edges
#'
#' @description Select edges in the network by SUID, name or other column values.
#' @param edges List of edge SUIDs, names or other column values
#' @param by.col Edge table column to lookup up provide edge values. Default is 'SUID'.
#' @param preserve.current.selection \code{boolean} Whether to maintain previously selected edges.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of newly selected edge SUIDs
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' selectEdges()
#' }
#' @export
selectEdges <-
    function(edges,
             by.col = 'SUID',
             preserve.current.selection = TRUE,
             network = NULL,
             base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        
        if (!preserve.current.selection)
            clearSelection(type = 'edges',
                           network = suid,
                           base.url = base.url)
        
        edge.list.str = NULL
        for (n in edges) {
            if (is.null(edge.list.str))
                edge.list.str = paste(by.col, n, sep = ":")
            else
                edge.list.str = paste(edge.list.str, paste(by.col, n, sep =
                                                               ":"), sep = ",")
        }
        res <-
            commandsPOST(
                paste0(
                    'network select network=SUID:"',
                    suid,
                    '" edgeList="',
                    edge.list.str
                ),
                base.url = base.url
            )
        return(res)
    }

# ------------------------------------------------------------------------------
#' Select all edges
#'
#' @description Selects all edges in a Cytoscape Network
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Selects all edges in a specified network.
#' @author Alexander Pico, Julia Gustavsen
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' selectAllEdges(cw)
#' }
#' @export
selectAllEdges <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    all_edge_SUIDs <-
        cyrestGET(paste('networks', suid, 'edges', sep = "/"), base.url = base.url)
    SUID.value.pairs <- lapply(all_edge_SUIDs,
                               function(s) {
                                   list('SUID' = s, 'value' = TRUE)
                               })
    res <-
        cyrestPUT(
            paste(
                'networks',
                suid,
                'tables/defaultedge/columns/selected',
                sep = "/"
            ),
            body = SUID.value.pairs,
            base.url = base.url
        )
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @title Invert Edge Selection
#'
#' @description Select all edges that were not selected and deselect all edges 
#' that were selected.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of newly selected edge SUIDs
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' invertEdgeSelection()
#' }
#' @export
invertEdgeSelection <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        res <-
            commandsPOST(paste0('network select invert=edges network=SUID:', suid),
                         base.url = base.url)
        return(res)
    }

# ------------------------------------------------------------------------------
#' @title Delete Selected Edges
#'
#' @description Delete the currently selected edges in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of deleted edge SUIDs
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' deleteSelectedEdges()
#' }
#' @export
deleteSelectedEdges <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        res <-
            commandsPOST(paste0('network delete edgeList=selected network=SUID:', suid),
                         base.url = base.url)
        return(res)
    }

# ------------------------------------------------------------------------------
#' @title Get Selected Edge Count
#'
#' @description Returns the number of edges currently selected in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{numeric}
#' @author AlexanderPico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getSelectedEdgeCount()
#' }
#' @export
getSelectedEdgeCount <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        net.SUID <- getNetworkSuid(network)
        res <- cyrestGET(
            paste("networks", net.SUID, "edges", sep = "/"),
            list(column = "selected", query = "true"),
            base.url = base.url
        )
        return(length(res))
    }

# ------------------------------------------------------------------------------
#' @title Get Selected Edges
#'
#' @description Retrieve the names of all the edges selected in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of selected edge names
#' @examples \donttest{
#' getSelectedEdges()
#' }
#' @export
getSelectedEdges <-
    function (network = NULL, base.url = .defaultBaseUrl) {
        net.SUID = getNetworkSuid(network)
        if (getSelectedEdgeCount(net.SUID, base.url) == 0) {
            return (NA)
        } else {
            selected.edges.SUIDs = cyrestGET(
                paste("networks", net.SUID, "edges", sep = "/"),
                list(column = "selected", query =
                         "true"),
                base.url = base.url
            )
            selected.edges = .edgeSUIDToEdgeName(selected.edges.SUIDs, net.SUID, base.url)
            return(selected.edges)
        }
    }

# ------------------------------------------------------------------------------
#' Select the edges connecting selected nodes in Cytoscape Network
#'
#' @description Selects edges in a Cytoscape Network connecting the selected nodes
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return network with edges selected
#' @examples \dontrun{
#' selectEdgesConnectingSelectedNodes()
#' }
#' @author Alexander Pico, Julia Gustavsen
#' @export

selectEdgesConnectingSelectedNodes <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        net.suid <- getNetworkSuid(network)
        selectedNodes = getSelectedNodes(net.suid, base.url)
        if (length (selectedNodes) == 1 && is.na (selectedNodes))
            return ()
        allEdges <- getAllEdges(net.suid, base.url)
        selectedSources <-
            unlist(mapply(function(x)
                return(allEdges [startsWith(allEdges, x)]), selectedNodes))
        selectedTargets <-
            unlist(mapply(function(x)
                return(allEdges [endsWith(allEdges, x)]), selectedNodes))
        selectedEdges <- intersect(selectedSources, selectedTargets)
        selectEdges(
            selectedEdges,
            preserve.current.selection = FALSE,
            network = net.suid,
            base.url = base.url
        )
    }

# ------------------------------------------------------------------------------
#' @title Select Edges Adjacent To Selected Nodes
#'
#' @description Takes currently selected nodes and adds to the selection all edges
#' connected to those nodes, regardless of directionality.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Lists of SUIDs for currently selected nodes and edges
#' @examples \donttest{
#' selectEdgesAdjacentToSelectedNodes()
#' }
#' @export
selectEdgesAdjacentToSelectedNodes <-
    function(network = NULL, base.url = .defaultBaseUrl) {
        suid <- getNetworkSuid(network)
        clearSelection(type = 'edges', suid, base.url)
        res <-
            commandsPOST(
                paste0(
                    'network select adjacentEdges="true" nodeList="selected network="',
                    suid,
                    '"'
                )
            )
        return(res)
    }


