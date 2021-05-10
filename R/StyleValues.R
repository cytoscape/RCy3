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
#' @param node.names List of node names or SUIDs. Default is NULL for all 
#' nodes.
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Named list of property values
#' @examples \donttest{
#' getNodeProperty(c('node 0','node 1'),'NODE_SHAPE')
#' }
#' @export
getNodeProperty <- function(node.names = NULL,
                            visual.property,
                            network = NULL,
                            base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network,base.url)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    if (is.null(node.names)){
        res <- cyrestGET(paste("networks",
                               net.SUID,
                               "views",
                               view.SUID,
                               "nodes",
                               sep = "/"),
                         c(visualProperty=visual.property),
                         base.url = base.url)
        node.suids <- unlist(lapply(res, function(x) x$SUID))
        node.props <- unlist(lapply(res, function(x) unlist(x$view)['value']))
        node.props <- setNames(node.props,.nodeSUIDToNodeName(node.suids))
        return(node.props)
    } else {
        node.SUIDs <-
            .nodeNameToNodeSUID(node.names, net.SUID, base.url)
        node.props <- c()
        for (i in seq_len(length(node.SUIDs))) {
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
            node.props <- c(node.props, unlist(unname(res['value'])))
        }
        node.props <- setNames(node.props, node.names)
        return(node.props)
    }
}

# ------------------------------------------------------------------------------
#' @title Get Edge Property Values
#'
#' @description Get values for any edge property of the specified edges.
#' @param edge.names List of edge names or SUIDs. Default is NULL for all edges.
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Named list of property values
#' @examples \donttest{
#' getEdgeProperty(c('node 0 (pp) node 1','node 0 (pp) node 2'),'EDGE_WIDTH')
#' }
#' @export
getEdgeProperty <- function(edge.names = NULL,
                            visual.property,
                            network = NULL,
                            base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network,base.url)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    if (is.null(edge.names)){
        res <- cyrestGET(paste("networks",
                               net.SUID,
                               "views",
                               view.SUID,
                               "edges",
                               sep = "/"),
                         c(visualProperty=visual.property),
                         base.url = base.url)
        edge.suids <- unlist(lapply(res, function(x) x$SUID))
        edge.props <- unlist(lapply(res, function(x) unlist(x$view)['value']))
        edge.props <- setNames(edge.props,.edgeSUIDToEdgeName(edge.suids))
        return(edge.props)
    } else {
        edge.SUIDs <-
            .edgeNameToEdgeSUID(edge.names, network=net.SUID, base.url=base.url)
        edge.props <- c()
        for (i in seq_len(length(edge.SUIDs))) {
            edge.SUID <- as.character(edge.SUIDs[i])
            res <- cyrestGET(paste( "networks",
                                    net.SUID,
                                    "views",
                                    view.SUID,
                                    "edges",
                                    edge.SUID,
                                    visual.property,
                                    sep = "/"),
                             base.url = base.url)
            edge.props <- c(edge.props, unlist(unname(res['value'])))
        }
        edge.props <- setNames(edge.props, edge.names)
        return(edge.props)
    }
}

# ------------------------------------------------------------------------------
#' @title Get Network Property Values
#'
#' @description Get values for any network property.
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method retrieves the actual property of the node, given the 
#' current visual style, factoring together any default, mapping and bypass setting.
#' @return Named list of property values
#' @examples \donttest{
#' getNetworkProperty('NETWORK_SCALE_FACTOR')
#' }
#' @export
getNetworkProperty <- function(visual.property,
                               network = NULL,
                               base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network,base.url)
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
    return(unlist(unname(res['value'])))
}

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. Node Properties
# Pattern: call getNodeProperty()
# ------------------------------------------------------------------------------
#' @title Get Node Color
#'
#' @description Retrieve the actual fill color of specified nodes.
#' @param node.names List of node names or SUIDs. Default is NULL for all nodes.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNodeColor()
#' }
#' @export
getNodeColor <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_FILL_COLOR", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Size
#'
#' @description Retrieve the actual size of specified nodes.
#' @param node.names List of node names or SUIDs. Default is NULL for all nodes.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNodeSize()
#' }
#' @export
getNodeSize <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_SIZE", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Width
#'
#' @description Retrieve the actual width of specified nodes.
#' @param node.names List of node names or SUIDs. Default is NULL for all nodes.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNodeWidth()
#' }
#' @export
getNodeWidth <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_WIDTH", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Height
#'
#' @description Retrieve the actual height of specified nodes.
#' @param node.names List of node names or SUIDs. Default is NULL for all nodes.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNodeHeight()
#' }
#' @export
getNodeHeight <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getNodeProperty(node.names, "NODE_HEIGHT", network=network, base.url = base.url)
}


# ------------------------------------------------------------------------------
#' @title Get Node Position
#'
#' @description Retrieve the actual x,y position of specified nodes.
#' @param node.names List of node names or SUIDs. Default is NULL for all nodes.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Dataframe of x, y values
#' @examples \donttest{
#' getNodePosition()
#' }
#' @export
getNodePosition <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    x_location <- getNodeProperty(node.names, "NODE_X_LOCATION", network=network, base.url = base.url)
    y_location <- getNodeProperty(node.names, "NODE_Y_LOCATION", network=network, base.url = base.url)
    node.pos <- merge(as.data.frame(x_location), as.data.frame(y_location), by = "row.names")
    row.names(node.pos) <- node.pos$Row.names
    node.pos[,-1]
}
 
# ==============================================================================
# II.b. Edge Properties
# Pattern: call getEdgeProperty()
# ------------------------------------------------------------------------------
#' @title Get Edge Line Width
#'
#' @description Retrieve the actual line width of specified edges.
#' @param edge.names List of edge names or SUIDs. Default is NULL for all edges.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getEdgeLineWidth()
#' }
#' @export
getEdgeLineWidth <- function (edge.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_WIDTH", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Color
#'
#' @description Retrieve the actual line color of specified edges.
#' @param edge.names List of edge names or SUIDs. Default is NULL for all edges.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getEdgeColor()
#' }
#' @export
getEdgeColor <- function (edge.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_PAINT", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Line Style
#'
#' @description Retrieve the actual line style of specified edges.
#' @param edge.names List of edge names or SUIDs. Default is NULL for all edges.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getEdgeLineStyle()
#' }
#' @export
getEdgeLineStyle <- function (edge.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_LINE_TYPE", network=network, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Target Arrow Shape
#'
#' @description Retrieve the actual target arrow shape of specified edges.
#' @param edge.names List of edge names or SUIDs. Default is NULL for all edges.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getEdgeTargetArrowShape()
#' }
#' @export
getEdgeTargetArrowShape <- function (edge.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    getEdgeProperty(edge.names, "EDGE_TARGET_ARROW_SHAPE", network=network, base.url = base.url)
}



# ==============================================================================
# II.c. Network Properties
# Pattern: call getNetworkProperty()
# ------------------------------------------------------------------------------
#' @title Get Network Center
#'
#' @description Retrieve the center of specified network.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNetworkCenter()
#' }
#' @export
getNetworkCenter <- function(network=NULL, base.url =.defaultBaseUrl) {
    x.coordinate <- getNetworkProperty("NETWORK_CENTER_X_LOCATION", 
                                       network = network, base.url = base.url)
    y.coordinate <- getNetworkProperty("NETWORK_CENTER_Y_LOCATION", 
                                       network = network, base.url = base.url)
    return(list(x = x.coordinate, y = y.coordinate))
}

# ------------------------------------------------------------------------------
#' @title Get Network Zoom
#'
#' @description Retrieve the scale factor of specified network.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Named list of property values
#' @examples \donttest{
#' getNetworkZoom()
#' }
#' @export
getNetworkZoom <- function(network=NULL, base.url =.defaultBaseUrl) {
    getNetworkProperty("NETWORK_SCALE_FACTOR", network = network, base.url = base.url)
}

