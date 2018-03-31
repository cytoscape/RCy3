# ==============================================================================
# Functions for setting and clearing BYPASS values for visual properties,
# organized into sections:
#
# I. General functions for setting/clearing node, edge and network properties
# II. Specific functions for setting particular node, edge and network properties
#
# NOTE: The CyREST 'bypass' enpoint is essential to properly set values that 
# will persist for a given network independent of applied style and style 
# changes, and from session to session if saved.
#
# ==============================================================================
# I. General Functions
# ==============================================================================
# I.a. Node Properties
# ------------------------------------------------------------------------------
#' @title Set Node Property Bypass
#'
#' @description Set bypass values for any node property of the specified nodes, 
#' overriding default values and mappings defined by any visual style.
#' @param node.names List of node names
#' @param new.values List of values to set, or single value
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param bypass Whether to set permanent bypass value. Default is \code{TRUE}. 
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for the visual properties of the node or nodes specified. To restore
#' defaults and mappings, use \link{clearNodePropertyBypass}.
#' @return None
#' @seealso \link{clearNodePropertyBypass}
#' @examples \donttest{
#' setNodePropertyBypass()
#' }
#' @export
setNodePropertyBypass <- function(node.names,
                                  new.values,
                                  visual.property,
                                  bypass = TRUE,
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    node.SUIDs <-
        .nodeNameToNodeSUID(node.names, network=net.SUID, base.url=base.url)
    # there can be more than one node.SUID per node.name!
    # 'node.SUIDs' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(node.SUIDs))
    }
    
    if (length(new.values) != length(node.SUIDs)) {
        write(
            sprintf(
                "ERROR in setNodePropertyBypass():\n   the number of nodes
                [%d] and new values [%d] are not the same >> node(s)
                attribute couldn't be set. Note that having multiple nodes 
                with the same name in the network can cause this error. Use
                node SUIDs or pass in duplicated names on their own.",
                length(node.SUIDs),
                length(new.values)
            ),
            stderr()
        )
        return()
    } else {
        for (i in 1:length(node.SUIDs)) {
            node.SUID <- as.character(node.SUIDs[i])
            new.value <- new.values[i]
            res <- cyrestPUT(paste("networks",
                                   net.SUID,
                                   "views",
                                   view.SUID,
                                   "nodes",
                                   node.SUID,
                                   sep = "/"),
                             parameters = list(bypass=bypass),
                             body = list(list(visualProperty = visual.property,
                                              value = new.value)),
                             base.url = base.url)
        }
    }
}
# ------------------------------------------------------------------------------
#' @title Clear Node Property Bypass
#'
#' @description Clear bypass values for any node property of the specified nodes,
#' effectively restoring any previously defined style defaults or mappings.
#' @param node.names List of node names
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @seealso \link{setNodePropertyBypass}
#' @examples \donttest{
#' clearNodePropertyBypass()
#' }
#' @export
clearNodePropertyBypass <-  function(node.names,
                                     visual.property,
                                     network = NULL,
                                     base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    node.SUIDs <-
        .nodeNameToNodeSUID(node.names, network=net.SUID, base.url=base.url)
    
    for (i in 1:length(node.SUIDs)) {
        node.SUID <- as.character(node.SUIDs[i])
        res <- cyrestDELETE( paste("networks",
                                   net.SUID,
                                   "views",
                                   view.SUID,
                                   "nodes",
                                   node.SUID,
                                   visual.property,
                                   "bypass",
                                   sep = "/"),
                             base.url = base.url )
    }
}

# ==============================================================================
# I.b. Edge Properties
# ------------------------------------------------------------------------------
#' @title Set Edge Property Bypass
#'
#' @description Set bypass values for any edge property of the specified edges, 
#' overriding default values and mappings defined by any visual style.
#' @param edge.names List of edge names
#' @param new.values List of values to set, or single value
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param bypass Whether to set permanent bypass value. Default is \code{TRUE}. 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for the visual properties of the edge or edges specified. To restore
#' defaults and mappings, use \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso \link{clearEdgePropertyBypass}
#' @examples \donttest{
#' setEdgePropertyBypass()
#' }
#' @export
setEdgePropertyBypass <- function(edge.names,
                                  new.values,
                                  visual.property,
                                  bypass = TRUE,
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    edge.SUIDs <-
        .edgeNameToEdgeSUID(edge.names, network=net.SUID, base.url=base.url)
    # 'edge.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(edge.names))
    }
    
    if (length(new.values) != length(edge.names)) {
        write(
            sprintf(
                "ERROR in setEdgePropertyBypass():\n\t number of
                edge.names [%d] and new.values [%d] are not the
                same >> edge(s) attribute could not be set",
                length(edge.names),
                length(new.values)
            ),
            stderr()
        )
    } else {
        for (i in 1:length(edge.SUIDs)) {
            edge.SUID <- as.character(edge.SUIDs[i])
            current.value <- new.values[i]
            
            res <- cyrestPUT(paste( "networks",
                                    net.SUID,
                                    "views",
                                    view.SUID,
                                    "edges",
                                    edge.SUID,
                                    sep = "/"),
                             parameters = list(bypass=bypass),
                             body = list(list(visualProperty = visual.property,
                                              value = current.value)),
                             base.url = base.url)
        }
    }
}
# ------------------------------------------------------------------------------
#' @title Clear Edge Property Bypass
#'
#' @description Clear bypass values for any edge property of the specified edges,
#' effectively restoring any previously defined style defaults or mappings.
#' @param edge.names List of edge names
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @seealso \link{setEdgePropertyBypass}
#' @examples \donttest{
#' clearEdgePropertyBypass()
#' }
#' @export
clearEdgePropertyBypass <- function(edge.names,
                                    visual.property,
                                    network = NULL,
                                    base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    edge.SUIDs <-
        .nodeNameToNodeSUID(edge.names, network=net.SUID, base.url=base.url)
    
    for (i in 1:length(edge.SUIDs)) {
        edge.SUID <- as.character(edge.SUIDs[i])
        res <- cyrestDELETE(paste( "networks",
                                   net.SUID,
                                   "views",
                                   view.SUID,
                                   "edges",
                                   edge.SUID,
                                   visual.property,
                                   "bypass",
                                   sep = "/"),
                            base.url = base.url)
    }
}

# ==============================================================================
# I.c. Network Properties
# ------------------------------------------------------------------------------
#' @title Set Network Property Bypass
#'
#' @description Set bypass values for any network property, overriding default 
#' values defined by any visual style.
#' @param new.value Value to set
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param bypass Whether to set permanent bypass value. Default is \code{TRUE}. 
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for the visual properties of the node or nodes specified. To restore
#' defaults and mappings, use \link{clearNodePropertyBypass}.
#' @return None
#' @seealso \link{clearNetworkPropertyBypass}
#' @examples \donttest{
#' setNetworkPropertyBypass()
#' }
#' @export
setNetworkPropertyBypass <- function(new.value,
                                     visual.property,
                                     bypass = TRUE,
                                     network = NULL,
                                     base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    res <- cyrestPUT(paste("networks",
                           net.SUID,
                           "views",
                           view.SUID,
                           "network",
                           sep = "/"),
                     parameters = list(bypass=bypass),
                     body = list(list(visualProperty = visual.property,
                                      value = new.value)),
                     base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Clear Network Property Bypass
#'
#' @description Clear bypass values for any network property,
#' effectively restoring any previously defined style defaults or mappings.
#' @param visual.property Name of a visual property. See \link{getVisualPropertyNames}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @seealso \link{setNodePropertyBypass}
#' @examples \donttest{
#' clearNetworkPropertyBypass()
#' }
#' @export
clearNetworkPropertyBypass <- function(visual.property,
                                       network = NULL,
                                       base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(network=net.SUID, base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    res <- cyrestDELETE( paste("networks",
                               net.SUID,
                               "views",
                               view.SUID,
                               "network",
                               visual.property, 
                               "bypass",
                               sep = "/"),
                         base.url = base.url)
}

# ==============================================================================
# II. Specific Functions
# ------------------------------------------------------------------------------
#' @title Unhide All
#'
#' @description Unhide all previously hidden nodes and edges, by 
#' clearing the Visible property bypass value.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{clearEdgePropertyBypass}, which 
#' can be used to clear any visual property. 
#' @return None
#' @seealso {
#' \link{clearEdgePropertyBypass},
#' \link{unhideNodes}
#' \link{unhideEdges}
#' }
#' @examples \donttest{
#' unhideAll()
#' }
#' @export
#
# DEV NOTE: unhideAll() is an exception to the pattern since it alternatively
# calls Node and Edge vesions of set***PropteryBypass().
#
unhideAll <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    node.names <- getAllNodes(net.SUID, base.url)
    clearNodePropertyBypass(node.names, "NODE_VISIBLE", network=net.SUID, base.url=base.url)
    
    edge.names <- getAllEdges(net.SUID, base.url)
    clearEdgePropertyBypass(edge.names, "EDGE_VISIBLE", network=net.SUID, base.url=base.url)
}

# ==============================================================================
# II.a. Node Properties
# Pattern: (1) validate input value, (2) call setNodePropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Node Color Bypass
#'
#' @description Set the bypass value for fill color for the specified node or nodes. 
#' @param node.names List of node names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeColorDirect ('node1', '#FF0088')
#' setNodeColorDirect (c('node1', 'node2'), c('#88FF88', '#FF0088'))
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_FILL_COLOR')
#' }
#' @export
setNodeColorBypass <-
    function (node.names,
              new.colors,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.color in new.colors) {
            # ensure the new color string is in correct hexadecimal format
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # set the node color bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.colors,
                "NODE_FILL_COLOR",
                network=network,
                base.url=base.url
            )
        )
    }
#-------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
#' @title Set Node Size Bypass
#'
#' @description Sets the bypass value of node size for one or more nodes. Only 
#' applicable if node dimensions are locked. See \code{lockNodeDimensions}.
#' @param node.names List of node names
#' @param new.sizes List of size values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeSizeBypass('Node 1', 35)
#' setNodeSizeBypass(c('Node 1','Node 2'), 35)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_SIZE')
#' }
#' @export
setNodeSizeBypass <- function (node.names,
                               new.sizes,
                               network = NULL,
                               base.url = .defaultBaseUrl) {
    
    for (current.size in new.sizes) {
        # ensure the sizes are numbers
        if (!is.double(current.size)) {
            write (
                sprintf (
                    'illegal size string "%s" in RCy3::setNodeSizeDirect.
                    It needs to be a number.',
                    current.size
                ),
                stderr ()
            )
            return ()
        }
    }
    # set the node properties bypass
    setNodePropertyBypass(node.names, new.sizes, "NODE_SIZE", network=network, base.url=base.url)
}
#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is not tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Width Bypass
#'
#' @description Override the width for particular nodes.
#' @param node.names List of node names
#' @param new.widths List of width values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeWidthBypass('Node 1', 35)
#' setNodeWidthBypass(c('Node 1','Node 2'), 35)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_WIDTH')
#' }
#' @export
setNodeWidthBypass <-
    function (node.names,
              new.widths,
              network = NULL,
              base.url = .defaultBaseUrl) {
        # unlock node dimensions
        lockNodeDimensions (FALSE)
        
        for (current.width in new.widths) {
            # ensure the width(s) are numbers
            if (!is.double(current.width)) {
                write (
                    sprintf (
                        'illegal node width "%s" in RCy3::setNodeWidthDirect.
                        Width needs to be a number.',
                        current.width
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node property bypass
        return(setNodePropertyBypass(node.names, new.widths, "NODE_WIDTH",
                                     network=network, base.url=base.url))
    }

#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Height Bypass
#'
#' @description Override the height for particular nodes.
#' @param node.names List of node names
#' @param new.heights List of height values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeHeightBypass('Node 1', 35)
#' setNodeHeightBypass(c('Node 1','Node 2'), 35)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_HEIGHT')
#' }
#' @export
setNodeHeightBypass <-
    function (node.names,
              new.heights,
              network = NULL,
              base.url = .defaultBaseUrl) {
        # unlock node dimensions
        lockNodeDimensions (FALSE)
        
        for (current.height in new.heights) {
            # ensure the height(s) are numbers
            if (!is.double(current.height)) {
                write (
                    sprintf (
                        'illegal height string "%s" in RCy3::setNodeHeightDirect.
                        It needs to be a number.',
                        current.height
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node property bypass
        return(
            setNodePropertyBypass(node.names, new.heights, "NODE_HEIGHT",
                                  network=network, base.url=base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Bypass
#'
#' @description Override the label for particular nodes.
#' @param node.names List of node names
#' @param new.labels List of labels, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeLabelBypass('Node 1', 'Custom Label')
#' setNodeLabelBypass(c('Node 1','Node 2'), 'Custom Label')
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_LABEL')
#' }
#' @export
setNodeLabelBypass <-
    function(node.names,
             new.labels,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyBypass(node.names, new.labels, "NODE_LABEL", network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Bypass
#'
#' @description Override the font size for particular nodes.
#' @param node.names List of node names
#' @param new.sizes List of size values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeFontSizeBypass('Node 1', 5)
#' setNodeFontSizeBypass(c('Node 1','Node 2'), 5)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_LABEL_FONT_SIZE')
#' }
#' @export
setNodeFontSizeBypass <-
    function(node.names,
             new.sizes,
             network = NULL,
             base.url = .defaultBaseUrl) {
        size.type.errors = 0
        
        for (current.size in new.sizes) {
            if (!is.double(current.size)) {
                write(
                    sprintf(
                        "ERROR in RCy3::setNodeFontSizeBypass():\n\t font size
                        '%s' has to be numerical value",
                        current.size
                    ),
                    stderr()
                )
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if (size.type.errors < 1) {
            setNodePropertyBypass(node.names,
                                  new.sizes,
                                  "NODE_LABEL_FONT_SIZE",
                                  network=network, base.url=base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Color Bypass
#'
#' @description Override the label color for particular nodes.
#' @param node.names List of node names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeLabelColorBypass('Node 1', '#FF55AA')
#' setNodeLabelColorBypass(c('Node 1','Node 2'), '#FF55AA')
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_LABEL_COLOR')
#' }
#' @export
setNodeLabelColorBypass <-
    function (node.names,
              new.colors,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.color in new.colors) {
            # ensure the color is formated in the correct hexadecimal style
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # set the node property bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.colors,
                "NODE_LABEL_COLOR",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Shape Bypass
#'
#' @description Override the shape for particular nodes.
#' @param node.names List of node names
#' @param new.shapes List of shapes, or single value. See \link{getNodeShapes}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeShapeBypass('Node 1', 'ROUND_RECTANGLE')
#' setNodeShapeBypass(c('Node 1','Node 2'), 'ROUND_RECTANGLE')
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_SHAPE')
#' }
#' @export
setNodeShapeBypass <-
    function (node.names,
              new.shapes,
              network = NULL,
              base.url = .defaultBaseUrl) {
        if (length (node.names) != length (new.shapes)) {
            if (length(new.shapes) != 1) {
                msg = sprintf (
                    'error in RCy3::setNodeShapeDirect.  new.shapes count
                    (%d) is neither 1 nor same as node.names count (%d)',
                    length (new.shapes),
                    length (node.names)
                )
                write (msg, stderr ())
                return ()
            }
        }
        
        # convert old to new node shapes
        new.shapes[new.shapes == 'round_rect'] <- 'ROUND_RECTANGLE'
        new.shapes[new.shapes == 'rect'] <- 'RECTANGLE'
        
        # ensure correct node shapes
        new.shapes <- toupper(new.shapes)
        unique.node.shapes <- unique(new.shapes)
        wrong.node.shape <- sapply(unique.node.shapes,
                                   function(x)
                                       ! (x %in% getNodeShapes(base.url)))
        if (any(wrong.node.shape)) {
            write (
                sprintf (
                    'ERROR in RCy3::setNodeShapeDirect. %s is not a valid
                    shape. Please note that some older shapes are no longer
                    available. For valid ones check getNodeShapes.',
                    new.shapes
                ),
                stderr ()
            )
            return(NA)
        }
        # set the node property bypass
        return(setNodePropertyBypass(node.names, new.shapes, "NODE_SHAPE",
                                     network=network, base.url=base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Bypass
#'
#' @description Override the border width for particular nodes.
#' @param node.names List of node names
#' @param new.sizes List of size values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeBorderWidthBypass('Node 1', 5)
#' setNodeBorderWidthBypass(c('Node 1','Node 2'), 5)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_BORDER_WIDTH')
#' }
#' @export
setNodeBorderWidthBypass <-
    function (node.names,
              new.sizes,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.size in new.sizes) {
            # ensure the widths are numbers
            if (!is.double(current.size)) {
                write (
                    sprintf (
                        'illegal width string "%s" in RCy3::setNodeBorderWidthDirect.
                        It needs to be a number.',
                        current.size
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node property bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.sizes,
                "NODE_BORDER_WIDTH",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Bypass
#'
#' @description Override the border color for particular nodes.
#' @param node.names List of node names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeBorderColorBypass('Node 1', '#FF55AA')
#' setNodeBorderColorBypass(c('Node 1','Node 2'), '#FF55AA')
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_BORDER_PAINT')
#' }
#' @export
setNodeBorderColorBypass <-
    function (node.names,
              new.colors,
              network = NULL,
              base.url = .defaultBaseUrl) {
        # ensure the color is formated in correct hexadecimal style
        for (color in new.colors) {
            if (.isNotHexColor(color)) {
                return()
            }
        }
        # set the node border color bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.colors,
                "NODE_BORDER_PAINT",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Opacity Bypass
#'
#' @description Set the bypass value for node fill, label and border opacity for 
#' the specified node or nodes.
#' @param node.names List of node names
#' @param new.values List of values to set, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodeOpacityBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodeOpacityBypass}
#' }
#' @examples \donttest{
#' setNodeOpacityBypass('Node 1', 100)
#' setNodeOpacityBypass(c('Node 1','Node 2'), 100)
#' clearNodeOpacityBypass(c('Node 1','Node 2'))
#' }
#' @export
setNodeOpacityBypass <-
    function (node.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.value in new.values) {
            # ensure the opacity value is a double and between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write (
                    sprintf (
                        'RCy3::setNodeOpacityDirect: illegal opacity string
                        "%s". It needs to be between 0 and 255.',
                        current.value
                    ),
                    stderr ()
                )
                return ()
            }
        }
        setNodePropertyBypass(node.names,
                              new.values,
                              "NODE_TRANSPARENCY",
                              network=network, base.url=base.url)
        setNodePropertyBypass(node.names,
                              new.values,
                              "NODE_BORDER_TRANSPARENCY",
                              network=network, base.url=base.url)
        setNodePropertyBypass(node.names,
                              new.values,
                              "NODE_LABEL_TRANSPARENCY",
                              network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Clear Node Opacity Bypass
#'
#' @description Clearn the bypass value for node fill, label and border opacity 
#' for the specified node or nodes, effectively restoring any previously defined 
#' style defaults or mappings.
#' @param node.names List of node names
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @seealso {
#' \link{setNodeOpacityBypass}
#' }
#' @examples \donttest{
#' clearNodeOpacityBypass(c('Node 1','Node 2'))
#' }
#' @export
clearNodeOpacityBypass <- function (node.names,
                                    network = NULL,
                                    base.url = .defaultBaseUrl) {
    
    clearNodePropertyBypass(node.names,
                            "NODE_TRANSPARENCY",
                            network=network, base.url=base.url)
    clearNodePropertyBypass(node.names,
                            "NODE_BORDER_TRANSPARENCY",
                            network=network, base.url=base.url)
    clearNodePropertyBypass(node.names,
                            "NODE_LABEL_TRANSPARENCY",
                            network=network, base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Fill Opacity Bypass
#'
#' @description Override the fill opacity for particular nodes.
#' @param node.names List of node names
#' @param new.values List of values to set, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}, see examples.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeFillOpacityBypass('Node 1', 100)
#' setNodeFillOpacityBypass(c('Node 1','Node 2'), 100)
#' clearNodePropertyBypass(c('Node 1','Node 2'), 'NODE_TRANSPARENCY')
#' }
#' @export
setNodeFillOpacityBypass <-
    function (node.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.value in new.values) {
            # ensure the opacity value is between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write (
                    sprintf (
                        'illegal opacity string "%s" in RCy3::setNodeFillOpacityDirect.
                        It needs to be a double and between 0 and 255.',
                        current.value
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node border color bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.values,
                "NODE_TRANSPARENCY",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Opacity Bypass
#'
#' @description Override the border opacity for particular nodes.
#' @param node.names List of node names
#' @param new.values List of values to set, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeBorderOpacityBypass()
#' }
#' @export
setNodeBorderOpacityBypass <-
    function (node.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.value in new.values) {
            # ensure the opacity value is a double and between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write (
                    sprintf (
                        'illegal opacity string "%s" in RCy3::setNodeBorderOpacityDirect.
                        It needs to be between 0 and 255.',
                        current.value
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node property bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.values,
                "NODE_BORDER_TRANSPARENCY",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Opacity Bypass
#'
#' @description Override the label opacity for particular nodes.
#' @param node.names List of node names
#' @param new.values List of values to set, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearNodePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{clearNodePropertyBypass}
#' }
#' @examples \donttest{
#' setNodeLabelOpacityBypass()
#' }
#' @export
setNodeLabelOpacityBypass <-
    function (node.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.value in new.values) {
            # ensure the opacity value is a double and between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write (
                    sprintf (
                        'illegal opacity string "%s" in RCy3::setNodeLabelOpacityDirect.
                        It needs to be between 0 and 255.',
                        current.value
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the node property bypass
        return(
            setNodePropertyBypass(
                node.names,
                new.values,
                "NODE_LABEL_TRANSPARENCY",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Hide Selected Nodes
#'
#' @description Hide (but do not delete) the currently selected nodes, by 
#' setting the Visible property bypass value to false.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#' \link{unhideNodes} or \link{unhideAll}.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{hideNodes},
#' \link{unhideNodes},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' hideSelectedNodes()
#' }
#' @export
hideSelectedNodes <-
    function (network = NULL, base.url = .defaultBaseUrl) {
        node.names <- getSelectedNodes(network, base.url)
        setNodePropertyBypass(node.names, 'false', "NODE_VISIBLE", network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Hide Nodes
#'
#' @description Hide (but do not delete) the specified node or nodes, by 
#' setting the Visible property bypass value to false.
#' @param node.names List of node names
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{setNodePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#' \link{unhideNodes} or \link{unhideAll}.
#' @return None
#' @seealso {
#' \link{setNodePropertyBypass},
#' \link{hideSelectedNodes},
#' \link{unhideNodes},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' hideNodes()
#' }
#' @export
hideNodes <-
    function(node.names,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyBypass(node.names, 'false', "NODE_VISIBLE", network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Unhide Nodes
#'
#' @description Unhide specified nodes that were previously hidden, by 
#' clearing the Visible property bypass value.
#' @param node.names List of node names
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the node or nodes specified. This method
#' ultimately calls the generic function, \link{clearNodePropertyBypass}, which 
#' can be used to clear any visual property. 
#' @return None
#' @seealso {
#' \link{clearNodePropertyBypass},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' unhideNodes()
#' }
#' @export
unhideNodes <-
    function(node.names,
             network = NULL,
             base.url = .defaultBaseUrl) {
        clearNodePropertyBypass(node.names, "NODE_VISIBLE", network=network, base.url=base.url)
    }

# ==============================================================================
# II.b. Edge Properties
# Pattern: (1) validate input value, (2) call setEdgePropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Edge Opacity Bypass
#'
#' @description Override the opacity for particular edges.
#' @param edge.names List of edge names
#' @param new.values List of values to set, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeOpacityBypass()
#' }
#' @export
setEdgeOpacityBypass <-
    function (edge.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.value in new.values) {
            # ensure the opacity value is a double and between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write (
                    sprintf (
                        'illegal opacity string "%s" in RCy3::setEdgeLabelOpacityDirect. It needs to be between 0 and 255.',
                        current.value
                    ),
                    stderr ()
                )
                return ()
            }
        }
        # set the edge property bypass
        #     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
        setEdgePropertyBypass(edge.names,
                              new.values,
                              "EDGE_LABEL_TRANSPARENCY",
                              network=network, base.url=base.url)
        setEdgePropertyBypass(edge.names,
                              new.values,
                              "EDGE_TRANSPARENCY",
                              network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
# Sets both edge color properties, independent on arrowColorMatchesEdge setting.
# ------------------------------------------------------------------------------
#' @title Set Edge Color Bypass
#'
#' @description Override the color for particular edges.
#' @param edge.names List of edge names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeColorBypass()
#' }
#' @export
setEdgeColorBypass <-
    function (edge.names,
              new.colors,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (color in new.colors) {
            if (.isNotHexColor(color)) {
                return()
            }
        }
        setEdgePropertyBypass(edge.names,
                              new.colors,
                              "EDGE_STROKE_UNSELECTED_PAINT",
                              network=network, base.url=base.url)
        setEdgePropertyBypass(edge.names,
                              new.colors,
                              "EDGE_UNSELECTED_PAINT",
                              network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Bypass
#'
#' @description Override the label for particular edges.
#' @param edge.names List of edge names
#' @param new.labels List of labels, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeLabelBypass()
#' }
#' @export
setEdgeLabelBypass <-
    function(edge.names,
             new.labels,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyBypass(edge.names, new.labels, "EDGE_LABEL", network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Face Bypass
#'
#' @description Override the font face for particular edges.
#' @param edge.names List of edge names
#' @param new.fonts List of font faces, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeFontFaceBypass()
#' }
#' @export
setEdgeFontFaceBypass <-
    function(edge.names,
             new.fonts,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyBypass(edge.names,
                              new.fonts,
                              "EDGE_LABEL_FONT_FACE",
                              network=network, base.url=base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Bypass
#'
#' @description Override the font size for particular edges.
#' @param edge.names List of edge names
#' @param new.sizes List of size values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeFontSizeBypass()
#' }
#' @export
setEdgeFontSizeBypass <-
    function(edge.names,
             new.sizes,
             network = NULL,
             base.url = .defaultBaseUrl) {
        size.type.errors = 0
        
        for (current.size in new.sizes) {
            # ensure the sizes are valid numbers
            if (!is.numeric(current.size)) {
                write(
                    sprintf (
                        'illegal font string "%s" in RCy3::setEdgeFontSizeBypass():\t\n it needs to be a valid number.',
                        current.size
                    ),
                    stderr ()
                )
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if (size.type.errors < 1) {
            setEdgePropertyBypass(edge.names,
                                  new.sizes,
                                  "EDGE_LABEL_FONT_SIZE",
                                  network=network, base.url=base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Color Bypass
#'
#' @description Override the label color for particular edges.
#' @param edge.names List of edge names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeLabelColorBypass()
#' }
#' @export
setEdgeLabelColorBypass <-
    function (edge.names,
              new.colors,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.color in new.colors) {
            # ensure the color is formated in correct hexadecimal style
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # set the edge property bypass
        return(
            setEdgePropertyBypass(
                edge.names,
                new.colors,
                "EDGE_LABEL_COLOR",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Tooltip Bypass
#'
#' @description Override the tooltip for particular edges.
#' @param edge.names List of edge names
#' @param new.values List of tooltip values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeTooltipBypass()
#' }
#' @export
setEdgeTooltipBypass <-
    function (edge.names,
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        if (length (edge.names) != length (new.values)) {
            if (length(new.values) != 1) {
                msg = sprintf (
                    'error in RCy3::setEdgeTooltipDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                    length (new.values),
                    length (edge.names)
                )
                write (msg, stderr ())
                return ()
            }
        }
        # set the edge property bypass
        return(
            setEdgePropertyBypass(edge.names, new.values, "EDGE_TOOLTIP", network=network, base.url=base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Bypass
#'
#' @description Override the width for particular edges.
#' @param edge.names List of edge names
#' @param new.widths List of width values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeLineWidthBypass()
#' }
#' @export
setEdgeLineWidthBypass <-
    function(edge.names,
             new.widths,
             network = NULL,
             base.url = .defaultBaseUrl) {
        for (current.size in new.widths) {
            # ensure the sizes are numbers
            if (!is.numeric(current.size)) {
                write (
                    sprintf (
                        'illegal size string "%s" in RCy3::setEdgeLineWidthDirect. It needs to be a number.',
                        current.size
                    ),
                    stderr ()
                )
                return ()
            }
        }
        
        # set the edge property bypass
        return(setEdgePropertyBypass(edge.names, new.widths, "EDGE_WIDTH", network=network, base.url=base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Bypass
#'
#' @description Override the style for particular edges.
#' @param edge.names List of edge names
#' @param new.styles List of style values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeLineStyleBypass()
#' }
#' @export
setEdgeLineStyleBypass <-
    function(edge.names,
             new.styles,
             network = NULL,
             base.url = .defaultBaseUrl) {
        unique.new.styles <- unique(new.styles)
        
        wrong.values <-
            sapply(unique.new.styles, function(v) {
                !(toupper(v) %in% getLineStyles(base.url))
            })
        
        if (any(wrong.values)) {
            error.msg <-
                paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])),
                      sep = "",
                      collapse = ", ")
            
            error.msg <-
                paste(
                    "\n\t\tERROR in setEdgeLineStyleBypass() >> INVALID line
                    style value(s): ",
                    error.msg,
                    "\n",
                    sep = ""
                )
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                toupper(new.styles),
                "EDGE_LINE_TYPE",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Shape Bypass
#'
#' @description Override the source arrow shape for particular edges.
#' @param edge.names List of edge names
#' @param new.shapes List of shapes, or single value. See \link{getArrowShapes}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeSourceArrowShapeBypass()
#' }
#' @export
setEdgeSourceArrowShapeBypass <-
    function(edge.names,
             new.shapes,
             network = NULL,
             base.url = .defaultBaseUrl) {
        unique.new.shapes <- unique(new.shapes)
        
        wrong.values <-
            sapply(unique.new.shapes, function(v) {
                !(toupper(v) %in% getArrowShapes(base.url))
            })
        
        if (any(wrong.values)) {
            error.msg <-
                paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])),
                      sep = "",
                      collapse = ", ")
            
            error.msg <-
                paste(
                    "\n\t\tERROR in setEdgeSourceArrowShapeBypass() >>
                    INVALID arrow shape value(s): ",
                    error.msg,
                    "\n",
                    sep = ""
                )
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                toupper(new.shapes),
                "EDGE_SOURCE_ARROW_SHAPE",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Shape Bypass
#'
#' @description Override the target arrow shape for particular edges.
#' @param edge.names List of edge names
#' @param new.shapes List of values to set, or single value. See \link{getArrowShapes}.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeTargetArrowShapeBypass()
#' }
#' @export
setEdgeTargetArrowShapeBypass <-
    function (edge.names,
              new.shapes,
              network = NULL,
              base.url = .defaultBaseUrl) {
        unique.new.shapes <- unique(new.shapes)
        
        wrong.values <-
            sapply(unique.new.shapes, function(v) {
                !(toupper(v) %in% getArrowShapes(base.url))
            })
        
        if (any(wrong.values)) {
            error.msg <-
                paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])),
                      sep = "",
                      collapse = ", ")
            
            error.msg <-
                paste(
                    "\n\t\tERROR in setEdgeTargetArrowShapeBypass() >> INVALID arrow shape value(s): ",
                    error.msg,
                    "\n",
                    sep = ""
                )
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                toupper(new.shapes),
                "EDGE_TARGET_ARROW_SHAPE",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Bypass
#'
#' @description Override the source arrow color for particular edges.
#' @param edge.names List of edge names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeSourceArrowColorBypass()
#' }
#' @export
setEdgeSourceArrowColorBypass <-
    function(edge.names,
             new.colors,
             network = NULL,
             base.url = .defaultBaseUrl) {
        for (current.color in new.colors) {
            # check the color is represented in hexadecimal format
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                new.colors,
                "EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Bypass
#'
#' @description Override the target arrow color for particular edges.
#' @param edge.names List of edge names
#' @param new.colors List of hex colors, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeTargetArrowColorBypass()
#' }
#' @export
setEdgeTargetArrowColorBypass <-
    function(edge.names,
             new.colors,
             network = NULL,
             base.url = .defaultBaseUrl) {
        for (current.color in new.colors) {
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                new.colors,
                "EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Opacity Bypass
#'
#' @description Override the label opacity for particular edges.
#' @param edge.names List of edge names
#' @param new.value List of opacity values, or single value
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#'  \link{clearEdgePropertyBypass}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{clearEdgePropertyBypass}
#' }
#' @examples \donttest{
#' setEdgeLabelOpacityBypass()
#' }
#' @export
setEdgeLabelOpacityBypass <-
    function(edge.names,
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        for (current.value in new.value) {
            # check that the opacity value is DOUBLE number between 0 and 255
            if (!is.double(current.value) ||
                current.value < 0  || current.value > 255) {
                write(
                    sprintf(
                        "\n\t\tERROR in setEdgeLabelOpacityBypass(): illegal opacity
                        value '%s'. Opacity needs to be number between 0 and 255",
                        current.value
                    ),
                    stderr()
                )
                
                return(FALSE)
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyBypass(
                edge.names,
                new.value,
                "EDGE_LABEL_TRANSPARENCY",
                network=network,
                base.url=base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Hide Selected Edges
#'
#' @description Hide (but do not delete) the currently selected edges, by 
#' setting the Visible property bypass value to false.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#' \link{unhideEdges} or \link{unhideAll}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{hideEdges},
#' \link{unhideEdges},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' hideSelectedEdges()
#' }
#' @export
hideSelectedEdges <- function (network=NULL, base.url = .defaultBaseUrl) {
    edge.names <- getSelectedEdges(base.url)
    setEdgePropertyBypass(edge.names, 'false', "EDGE_VISIBLE", network=network, base.url=base.url)
} 

# ------------------------------------------------------------------------------
#' @title Hide Edges
#'
#' @description Hide (but do not delete) the specified edge or edges, by 
#' setting the Visible property bypass value to false.
#' @param edge.names List of edge names
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{setEdgePropertyBypass}, which 
#' can be used to set any visual property. To restore defaults and mappings, use
#' \link{unhideEdges} or \link{unhideAll}.
#' @return None
#' @seealso {
#' \link{setEdgePropertyBypass},
#' \link{hideSelectedEdges},
#' \link{unhideEdges},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' hideEdges()
#' }
#' @export
hideEdges <- function (edge.names, network=NULL, base.url = .defaultBaseUrl) {
    setEdgePropertyBypass(edge.names, 'false', "EDGE_VISIBLE", network=network, base.url=base.url)
} 

# ------------------------------------------------------------------------------
#' @title Unhide Edges
#'
#' @description Unhide specified edges that were previously hidden, by 
#' clearing the Visible property bypass value.
#' @param edge.names List of edge names
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values or mappings 
#' defined for this visual property of the edge or edges specified. This method
#' ultimately calls the generic function, \link{clearEdgePropertyBypass}, which 
#' can be used to clear any visual property. 
#' @return None
#' @seealso {
#' \link{clearEdgePropertyBypass},
#' \link{unhideAll}
#' }
#' @examples \donttest{
#' unhideEdges()
#' }
#' @export
unhideEdges <- function (edge.names, network=NULL, base.url = .defaultBaseUrl) {
    clearEdgePropertyBypass(edge.names, "EDGE_VISIBLE", network=network, base.url=base.url)
} 

# ==============================================================================
# II.c. Network Properties
# Pattern: (1) validate input value, (2) call setNetworkPropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Network Zoom Bypass
#'
#' @description Set the bypass value for scale factor for the network.
#' @param new.value Zoom factor
#' @param bypass Whether to set permanent bypass value. Default is \code{FALSE} 
#' per common use of temporary zoom settings.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values 
#' for this visual property. This method
#' ultimately calls the generic function, \link{setNetworkPropertyBypass}, which 
#' can be used to set any visual property. To restore defaults, use
#'  \link{clearNetworkPropertyBypass}.
#' @return None
#' @seealso {
#' \link{setNetworkPropertyBypass},
#' \link{clearNetworkPropertyBypass}
#' }
#' @examples \donttest{
#' setNetworkZoomBypass()
#' }
#' @export
setNetworkZoomBypass <- function(new.value, bypass = FALSE, network = NULL, base.url = .defaultBaseUrl) {
    setNetworkPropertyBypass(new.value, "NETWORK_SCALE_FACTOR", bypass=bypass, network=network, base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Clear Network Zoom Bypass
#'
#' @description Clear the bypass value for the scale factor for the network, 
#' effectively restoring prior default values.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' clearNetworkZoomBypass()
#' }
#' @export
clearNetworkZoomBypass <- function(network = NULL, base.url = .defaultBaseUrl) {
    clearNetworkPropertyBypass("NETWORK_SCALE_FACTOR", network=network, base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Network Center Bypass
#'
#' @description Set the bypass value for center x and y for the network. This
#' function could be used to pan and scroll the Cytoscape canvas.
#' @param x Coordinate value, increases going to the right.
#' @param y Coordinate value, increase going down.
#' @param bypass Whether to set permanent bypass value. Default is \code{FALSE} 
#' per common use of temporary center settings.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details This method permanently overrides any default values 
#' for this visual property. This method
#' ultimately calls the generic function, \link{setNetworkPropertyBypass}, which 
#' can be used to set any visual property. To restore defaults, use
#'  \link{clearNetworkPropertyBypass}.
#' @return None
#' @seealso {
#' \link{setNetworkPropertyBypass},
#' \link{clearNetworkPropertyBypass}
#' }
#' @examples \donttest{
#' setNetworkCenterBypass()
#' }
#' @export
setNetworkCenterBypass <- function(x, y, bypass = FALSE, network = NULL, base.url = .defaultBaseUrl) {
    setNetworkPropertyBypass(x, "NETWORK_CENTER_X_LOCATION", bypass=bypass, network=network, base.url=base.url)
    setNetworkPropertyBypass(y, "NETWORK_CENTER_Y_LOCATION", bypass=bypass, network=network, base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Clear Network Center Bypass
#'
#' @description Clear the bypass value for center x and y for the network, 
#' effectively restoring prior default values.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' clearNetworkCenterBypass()
#' }
#' @export
clearNetworkCenterBypass <- function(network = NULL, base.url = .defaultBaseUrl) {
    clearNetworkPropertyBypass("NETWORK_CENTER_X_LOCATION", network=network, base.url=base.url)
    clearNetworkPropertyBypass("NETWORK_CENTER_Y_LOCATION", network=network, base.url=base.url)
}

