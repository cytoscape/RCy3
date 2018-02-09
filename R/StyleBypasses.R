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
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param visual.property DESCRIPTION
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
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    node.SUIDs <-
        .nodeNameToNodeSUID(node.names, net.SUID, base.url)
    
    # 'node.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(node.names))
    }
    
    if (length(new.values) != length(node.names)) {
        write(
            sprintf(
                "ERROR in setNodePropertyBypass():\n   the number of nodes
                    [%d] and new values [%d] are not the same >> node(s)
                    attribute couldn't be set",
                length(node.names),
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
                                   visual.property,
                                   'bypass',
                                   sep = "/"),
                             body = list(visualProperty = visual.property,
                                         value = new.value),
                             base.url = base.url)
        }
    }
}
# ------------------------------------------------------------------------------
#' @title Clear Node Property Bypass
#'
#' @description Clear bypass values for any node property of the specified nodes,
#' effectively restoring any previously defined style defaults or mappings.
#' @param node.names DESCRIPTION
#' @param visual.property DESCRIPTION
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
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    node.SUIDs <-
        .nodeNameToNodeSUID(node.names, net.SUID, base.url)
    
    for (i in 1:length(node.SUIDs)) {
        node.SUID <- as.character(node.SUIDs[i])
        new.value <- new.values[i]
        res <- cyrestDELETE( paste("networks",
                                   net.SUID,
                                   "views",
                                   view.SUID,
                                   "nodes",
                                   node.SUID,
                                   visual.property,
                                   'bypass',
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
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param visual.property DESCRIPTION
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
                                  network = NULL,
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    edge.SUIDs <-
        .edgeNameToEdgeSUID(edge.names, net.SUID, base.url)
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
                                    visual.property,
                                    'bypass',
                                    sep = "/"),
                             body = list(visualProperty = visual.property,
                                         value = current.value),
                             base.url = base.url)
        }
    }
}
# ------------------------------------------------------------------------------
#' @title Clear Edge Property Bypass
#'
#' @description Clear bypass values for any edge property of the specified edges,
#' effectively restoring any previously defined style defaults or mappings.
#' @param edge.names DESCRIPTION
#' @param visual.property DESCRIPTION
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
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    edge.SUIDs <-
        .nodeNameToNodeSUID(edge.names, net.SUID, base.url)
    
    for (i in 1:length(edge.SUIDs)) {
        edge.SUID <- as.character(edge.SUIDs[i])
        new.value <- new.values[i]
        res <- cyrestDELETE(paste( "networks",
                                   net.SUID,
                                   "views",
                                   view.SUID,
                                   "edges",
                                   edge.SUID,
                                   visual.property,
                                   'bypass',
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
#' @param new.value DESCRIPTION
#' @param visual.property DESCRIPTION
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
                                     network = NULL,
                                     base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    res <- cyrestPUT(paste("networks",
                           net.SUID,
                           "views",
                           view.SUID,
                           "network",
                           sep = "/"),
                     parameters = list(bypass='true'),
                     body = list(list(visualProperty = visual.property,
                                 value = new.value)),
                     base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Clear Network Property Bypass
#'
#' @description Clear bypass values for any network property,
#' effectively restoring any previously defined style defaults or mappings.
#' @param visual.property DESCRIPTION
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
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    res <- cyrestDELETE( paste("networks",
                               net.SUID,
                               "views",
                               view.SUID,
                               "network",
                               visual.property, ## NOT AVAILABLE IN CYREST YET!
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
    suid <- getNetworkSuid(network)
    node.names <- getAllNodes(suid, base.url)
    clearNodePropertyBypass(node.names, "NODE_VISIBLE", network, base.url)
    
    edge.names <- getAllEdges(suid, base.url)
    clearEdgePropertyBypass(edge.names, "EDGE_VISIBLE", network, base.url)
}

# ==============================================================================
# II.a. Node Properties
# Pattern: (1) validate input value, (2) call setNodePropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Node Color Bypass
#'
#' @description Set the bypass value for fill color for the specified node or nodes. 
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
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
#' setNodeColorDirect ('node1', '#FF0088')
#' setNodeColorDirect (c('node1', 'node2'), c('#88FF88', '#FF0088'))
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
                network,
                base.url
            )
        )
    }
#-------------------------------------------------------------------------------
# only works if node dimensions are unlocked (that is not tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Size Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
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
#' setNodeSizeBypass()
#' }
#' @export
setNodeSizeBypass <- function (node.names,
                               new.sizes,
                               network = NULL,
                               base.url = .defaultBaseUrl) {
    # unlock node dimensions
    lockNodeDimensions (FALSE)
    
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
    setNodePropertyBypass(node.names, new.sizes, "NODE_WIDTH", network, base.url)
    setNodePropertyBypass(node.names, new.sizes, "NODE_HEIGHT", network, base.url)
}
#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is not tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Width Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.widths DESCRIPTION
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
#' setNodeWidthBypass()
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
                                     network, base.url))
    }

#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Height Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.heights DESCRIPTION
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
#' setNodeHeightBypass()
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
                                  network, base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.labels DESCRIPTION
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
#' setNodeLabelBypass()
#' }
#' @export
setNodeLabelBypass <-
    function(node.names,
             new.labels,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyBypass(node.names, new.labels, "NODE_LABEL", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
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
#' setNodeFontSizeBypass()
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
                                  network,
                                  base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
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
#' setNodeLabelColorBypass()
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Shape Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.shapes DESCRIPTION
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
#' setNodeShapeBypass()
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
                                     network, base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
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
#' setNodeBorderWidthBypass()
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
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
#' setNodeBorderColorBypass()
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
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
#' setNodeOpacityBypass()
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
                              network,
                              base.url)
        setNodePropertyBypass(node.names,
                              new.values,
                              "NODE_BORDER_TRANSPARENCY",
                              network,
                              base.url)
        setNodePropertyBypass(node.names,
                              new.values,
                              "NODE_LABEL_TRANSPARENCY",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Node Fill Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
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
#' setNodeFillOpacityBypass()
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
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
                network,
                base.url
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
        setNodePropertyBypass(node.names, 'false', "NODE_VISIBLE", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Hide Nodes
#'
#' @description Hide (but do not delete) the specified node or nodes, by 
#' setting the Visible property bypass value to false.
#' @param node.names DESCRIPTION
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
        setNodePropertyBypass(node.names, 'false', "NODE_VISIBLE", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Unhide Nodes
#'
#' @description Unhide specified nodes that were previously hidden, by 
#' clearing the Visible property bypass value.
#' @param node.names DESCRIPTION
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
        clearNodePropertyBypass(node.names, "NODE_VISIBLE", network, base.url)
    }

# ==============================================================================
# II.b. Edge Properties
# Pattern: (1) validate input value, (2) call setEdgePropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Edge Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
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
                              network,
                              base.url)
        setEdgePropertyBypass(edge.names,
                              new.values,
                              "EDGE_TRANSPARENCY",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
# Sets both edge color properties, independent on arrowColorMatchesEdge setting.
# ------------------------------------------------------------------------------
#' @title Set Edge Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
              new.value,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (color in new.value) {
            if (.isNotHexColor(color)) {
                return()
            }
        }
        setEdgePropertyBypass(edge.names,
                              new.value,
                              "EDGE_STROKE_UNSELECTED_PAINT",
                              network,
                              base.url)
        setEdgePropertyBypass(edge.names,
                              new.value,
                              "EDGE_UNSELECTED_PAINT",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyBypass(edge.names, new.value, "EDGE_LABEL", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Face Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyBypass(edge.names,
                              new.value,
                              "EDGE_LABEL_FONT_FACE",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        size.type.errors = 0
        
        for (current.size in new.value) {
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
                                  new.value,
                                  "EDGE_LABEL_FONT_SIZE",
                                  network,
                                  base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
              new.value,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (current.color in new.value) {
            # ensure the color is formated in correct hexadecimal style
            if (.isNotHexColor(current.color)) {
                return()
            }
        }
        # set the edge property bypass
        return(
            setEdgePropertyBypass(
                edge.names,
                new.value,
                "EDGE_LABEL_COLOR",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Tooltip Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
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
            setEdgePropertyBypass(edge.names, new.values, "EDGE_TOOLTIP", network, base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        for (current.size in new.value) {
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
        return(setEdgePropertyBypass(edge.names, new.value, "EDGE_WIDTH", network, base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
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
             new.values,
             network = NULL,
             base.url = .defaultBaseUrl) {
        unique.new.values <- unique(new.values)
        
        wrong.values <-
            sapply(unique.new.values, function(v) {
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
                toupper(new.values),
                "EDGE_LINE_TYPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Shape Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
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
             new.values,
             network = NULL,
             base.url = .defaultBaseUrl) {
        unique.new.values <- unique(new.values)
        
        wrong.values <-
            sapply(unique.new.values, function(v) {
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
                toupper(new.values),
                "EDGE_SOURCE_ARROW_SHAPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Shape Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
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
              new.values,
              network = NULL,
              base.url = .defaultBaseUrl) {
        unique.new.values <- unique(new.values)
        
        wrong.values <-
            sapply(unique.new.values, function(v) {
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
                toupper(new.values),
                "EDGE_TARGET_ARROW_SHAPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.colors DESCRIPTION
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.colors DESCRIPTION
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
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Opacity Bypass
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
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
                network,
                base.url
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
    setEdgePropertyBypass(edge.names, 'false', "EDGE_VISIBLE", network, base.url)
} 

# ------------------------------------------------------------------------------
#' @title Hide Edges
#'
#' @description Hide (but do not delete) the specified edge or edges, by 
#' setting the Visible property bypass value to false.
#' @param edge.names DESCRIPTION
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
    setEdgePropertyBypass(edge.names, 'false', "EDGE_VISIBLE", network, base.url)
} 

# ------------------------------------------------------------------------------
#' @title Unhide Edges
#'
#' @description Unhide specified edges that were previously hidden, by 
#' clearing the Visible property bypass value.
#' @param edge.names DESCRIPTION
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
    clearEdgePropertyBypass(edge.names, "EDGE_VISIBLE", network, base.url)
} 

# ==============================================================================
# II.c. Network Properties
# Pattern: (1) validate input value, (2) call setNetworkPropertyBypass()
# ------------------------------------------------------------------------------
#' @title Set Network Zoom Bypass
#'
#' @description Set the bypass value for scale factor for the network.
#' @param new.value DESCRIPTION
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
setNetworkZoomBypass <- function(new.value, network = NULL, base.url = .defaultBaseUrl) {
    setNetworkPropertyBypass(new.value, "NETWORK_SCALE_FACTOR", network, base.url)
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
    clearNetworkPropertyBypass("NETWORK_SCALE_FACTOR", network, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Network Center Bypass
#'
#' @description Set the bypass value for center x and y for the network. This
#' function could be used to pan and scroll the Cytoscape canvas.
#' @param x DESCRIPTION
#' @param y DESCRIPTION
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
setNetworkCenterBypass <- function(x, y, network = NULL, base.url = .defaultBaseUrl) {
    setNetworkPropertyBypass(x, "NETWORK_CENTER_X_LOCATION", network, base.url)
    setNetworkPropertyBypass(y, "NETWORK_CENTER_Y_LOCATION", network, base.url)
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
    clearNetworkPropertyBypass("NETWORK_CENTER_X_LOCATION", network, base.url)
    clearNetworkPropertyBypass("NETWORK_CENTER_Y_LOCATION", network, base.url)
}

