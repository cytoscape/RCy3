# ==============================================================================
# Functions for retrieving current values for visual properties.
#
# I. General functions for getting node, edge and network properties
# II. Specific functions for getting particular node, edge and network properties
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' @title Get Node Property Values
#'
#' @description Get values for any node property of the specified nodes.
#' @param node.names DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Property value
#' @examples \donttest{
#' getNodeProperty()
#' }
#' @export
getNodeProperty <- function(node.names,
                                  visual.property,
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    node.SUIDs <-
        .nodeNameToNodeSUID(node.names, net.SUID, base.url)
    
    for (i in 1:length(node.SUIDs)) {
        node.SUID <- as.character(node.SUIDs[i])
        res <- cyrestGET(paste("networks",
                               net.SUID,
                               "views",
                               view.SUID,
                               "nodes",
                               node.SUID,
                               visual.property,
                               sep = "/"),
                         base.url = base.url)
    }
}

# ------------------------------------------------------------------------------
#' @title Get Edge Property Values
#'
#' @description Get values for any edge property of the specified edges.
#' @param edge.names DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Property value
#' @examples \donttest{
#' getEdgeProperty()
#' }
#' @export
getEdgeProperty <- function(edge.names,
                                  visual.property,
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    edge.SUIDs <-
        .edgeNameToEdgeSUID(edge.names, net.SUID, base.url)
    
    for (i in 1:length(edge.SUIDs)) {
        edge.SUID <- as.character(edge.SUIDs[i])
        current.value <- new.values[i]
        
        res <- cyrestGET(paste( "networks",
                                net.SUID,
                                "views",
                                view.SUID,
                                "edges",
                                edge.SUID,
                                visual.property,
                                sep = "/"),
                         base.url = base.url)
        
    }
}

# ------------------------------------------------------------------------------
#' @title Get Network Property Values
#'
#' @description Get values for any network property.
#' @param visual.property DESCRIPTION
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Property value
#' @examples \donttest{
#' getNetworkProperty()
#' }
#' @export
getNetworkProperty <- function(visual.property,
                                     network = NULL,
                                     base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    res <- cyrestGET(paste("networks",
                           net.SUID,
                           "views",
                           view.SUID,
                           "network",
                           visual.property,
                           sep = "/"),
                     base.url = base.url)
}

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. Node Properties
# Pattern: call getNodeProperty()
# ------------------------------------------------------------------------------
#' @export
getNodeSize <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_SIZE", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getNodeWidth <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_WIDTH", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getNodeHeight <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_HEIGHT", network=network, base.url = base.url)
}

# ==============================================================================
# II.b. Edge Properties
# Pattern: call getEdgeProperty()
# ------------------------------------------------------------------------------
#' @export
getEdgeLineWidth <- function (egde.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_WIDTH", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getEdgeColor <- function (egde.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_PAINT", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getEdgeLineStyle <- function (egde.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_LINE_TYPE", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getEdgeTargetArrowShape <- function (egde.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_TARGET_ARROW_SHAPE", network=network, base.url = base.url)
}



# ==============================================================================
# II.c. Network Properties
# Pattern: call getNetworkProperty()
# ------------------------------------------------------------------------------
#' @export
getNetworkCenter <- function(network=NULL, base.url =.defaultBaseUrl) {
    x.coordinate <- getNetworkProperty("NETWORK_CENTER_X_LOCATION", 
                                       network = network, base.url = base.url)
    y.coordinate <- getNetworkProperty("NETWORK_CENTER_Y_LOCATION", 
                                       network = network, base.url = base.url)
    return(list(x = x.coordinate$value, y = y.coordinate$value))
}

# ------------------------------------------------------------------------------
#' @export
getNetworkZoom <- function(network=NULL, base.url =.defaultBaseUrl) {
    getNetworkProperty("NETWORK_SCALE_FACTOR", network = network, base.url = base.url)
}

