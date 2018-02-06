# ==============================================================================
# Functions for setting and clearing BYPASS values for visual properties,
# organized into sections:
#
# I. General functions for setting/clearing node and edge properties
# II. Specific functions for setting particular node and edge properties
#
# NOTE: The 'bypass' enpoint is essential to properly set values that will
# persist for a given network independent of applied style and style changes.
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' @title Set Node Property Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodePropertyDirect()
#' }
#' @export
setNodePropertyDirect <-
    function(node.names,
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
                    "ERROR in setNodePropertyDirect():\n   the number of nodes
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
                res <-
                    cyrestPUT(
                        paste(
                            "networks",
                            net.SUID,
                            "views",
                            view.SUID,
                            "nodes",
                            node.SUID,
                            visual.property,
                            'bypass',
                            sep = "/"
                        ),
                        body = list(visualProperty = visual.property,
                                    value = new.value),
                        base.url = base.url
                    )
            }
        }
    }
# ------------------------------------------------------------------------------
#' @title Clear Node Property Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' clearNodePropertyDirect()
#' }
#' @export
clearNodePropertyDirect <-
    function(node.names,
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
            res <-
                cyrestDELETE(
                    paste(
                        "networks",
                        net.SUID,
                        "views",
                        view.SUID,
                        "nodes",
                        node.SUID,
                        visual.property,
                        'bypass',
                        sep = "/"
                    ),
                    base.url = base.url
                )
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Property Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgePropertyDirect()
#' }
#' @export
setEdgePropertyDirect <-
    function(edge.names,
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
                    "ERROR in setEdgePropertyDirect():\n\t number of
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
                
                res <-
                    cyrestPUT(
                        paste(
                            "networks",
                            net.SUID,
                            "views",
                            view.SUID,
                            "edges",
                            edge.SUID,
                            visual.property,
                            'bypass',
                            sep = "/"
                        ),
                        body = list(visualProperty = visual.property,
                                    value = current.value),
                        base.url = base.url
                    )
            }
        }
    }
# ------------------------------------------------------------------------------
#' @title Clear Edge Property Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param visual.property DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' clearEdgePropertyDirect()
#' }
#' @export
clearEdgePropertyDirect <-
    function(edge.names,
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
            res <-
                cyrestDELETE(
                    paste(
                        "networks",
                        net.SUID,
                        "views",
                        view.SUID,
                        "nodes",
                        edge.SUID,
                        visual.property,
                        'bypass',
                        sep = "/"
                    ),
                    base.url = base.url
                )
        }
    }

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. NODE PROPERTIES
# Pattern: (1) validate input value, (2) call setNodePropertyDirect()
#
# ------------------------------------------------------------------------------
#' @title Set Node Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeColorDirect()
#' }
#' @export
setNodeColorDirect <-
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
        # set the node color direct
        return(
            setNodePropertyDirect(
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
#' @title Set Node Size Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeSizeDirect()
#' }
#' @export
setNodeSizeDirect <- function (node.names,
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
    # set the node properties direct
    setNodePropertyDirect(node.names, new.sizes, "NODE_WIDTH", network, base.url)
    setNodePropertyDirect(node.names, new.sizes, "NODE_HEIGHT", network, base.url)
}
#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is not tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Width Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.widths DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeWidthDirect()
#' }
#' @export
setNodeWidthDirect <-
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
        # set the node property direct
        return(setNodePropertyDirect(node.names, new.widths, "NODE_WIDTH",
                                     network, base.url))
    }

#-------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).
# See lockNodeDimensions (T/F)
# ------------------------------------------------------------------------------
#' @title Set Node Height Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.heights DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeHeightDirect()
#' }
#' @export
setNodeHeightDirect <-
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
        # set the node property direct
        return(
            setNodePropertyDirect(node.names, new.heights, "NODE_HEIGHT",
                                  network, base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.labels DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeLabelDirect()
#' }
#' @export
setNodeLabelDirect <-
    function(node.names,
             new.labels,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyDirect(node.names, new.labels, "NODE_LABEL", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeFontSizeDirect()
#' }
#' @export
setNodeFontSizeDirect <-
    function(node.names,
             new.sizes,
             network = NULL,
             base.url = .defaultBaseUrl) {
        size.type.errors = 0
        
        for (current.size in new.sizes) {
            if (!is.double(current.size)) {
                write(
                    sprintf(
                        "ERROR in RCy3::setNodeFontSizeDirect():\n\t font size
                        '%s' has to be numerical value",
                        current.size
                    ),
                    stderr()
                )
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if (size.type.errors < 1) {
            setNodePropertyDirect(node.names,
                                  new.sizes,
                                  "NODE_LABEL_FONT_SIZE",
                                  network,
                                  base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeLabelColorDirect()
#' }
#' @export
setNodeLabelColorDirect <-
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
        # set the node property direct
        return(
            setNodePropertyDirect(
                node.names,
                new.colors,
                "NODE_LABEL_COLOR",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Shape Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.shapes DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeShapeDirect()
#' }
#' @export
setNodeShapeDirect <-
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
        # set the node property direct
        return(setNodePropertyDirect(node.names, new.shapes, "NODE_SHAPE",
                                     network, base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.sizes DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeBorderWidthDirect()
#' }
#' @export
setNodeBorderWidthDirect <-
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
        # set the node property direct
        return(
            setNodePropertyDirect(
                node.names,
                new.sizes,
                "NODE_BORDER_WIDTH",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.colors DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeBorderColorDirect()
#' }
#' @export
setNodeBorderColorDirect <-
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
        # set the node border color direct
        return(
            setNodePropertyDirect(
                node.names,
                new.colors,
                "NODE_BORDER_PAINT",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeOpacityDirect()
#' }
#' @export
setNodeOpacityDirect <-
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
        setNodePropertyDirect(node.names,
                              new.values,
                              "NODE_TRANSPARENCY",
                              network,
                              base.url)
        setNodePropertyDirect(node.names,
                              new.values,
                              "NODE_BORDER_TRANSPARENCY",
                              network,
                              base.url)
        setNodePropertyDirect(node.names,
                              new.values,
                              "NODE_LABEL_TRANSPARENCY",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Node Fill Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeFillOpacityDirect()
#' }
#' @export
setNodeFillOpacityDirect <-
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
        # set the node border color direct
        return(
            setNodePropertyDirect(
                node.names,
                new.values,
                "NODE_TRANSPARENCY",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Border Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeBorderOpacityDirect()
#' }
#' @export
setNodeBorderOpacityDirect <-
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
        # set the node property direct
        return(
            setNodePropertyDirect(
                node.names,
                new.values,
                "NODE_BORDER_TRANSPARENCY",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Node Label Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeLabelOpacityDirect()
#' }
#' @export
setNodeLabelOpacityDirect <-
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
        # set the node property direct
        return(
            setNodePropertyDirect(
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
#' @description FUNCTION_DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' hideSelectedNodes()
#' }
#' @export
hideSelectedNodes <-
    function (network = NULL, base.url = .defaultBaseUrl) {
        node.names <- getSelectedNodes(network, base.url)
        setNodePropertyDirect(node.names, 'false', "NODE_VISIBLE", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Hide Nodes
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' hideNodes()
#' }
#' @export
hideNodes <-
    function(node.names,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyDirect(node.names, 'false', "NODE_VISIBLE", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Unhide Nodes
#'
#' @description FUNCTION_DESCRIPTION
#' @param node.names DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' unhideNodes()
#' }
#' @export
unhideNodes <-
    function(node.names,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setNodePropertyDirect(node.names, 'true', "NODE_VISIBLE", network, base.url)
    }

# ==============================================================================
# II.a. EDGE PROPERTIES
# Pattern: (1) validate input value, (2) call setEdgePropertyDirect()
#
# ------------------------------------------------------------------------------
#' @title Set Edge Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeOpacityDirect()
#' }
#' @export
setEdgeOpacityDirect <-
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
        # set the edge property direct
        #     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
        setEdgePropertyDirect(edge.names,
                              new.values,
                              "EDGE_LABEL_TRANSPARENCY",
                              network,
                              base.url)
        setEdgePropertyDirect(edge.names,
                              new.values,
                              "EDGE_TRANSPARENCY",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
# Sets both edge color properties, independent on arrowColorMatchesEdge setting.
# ------------------------------------------------------------------------------
#' @title Set Edge Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeColorDirect()
#' }
#' @export
setEdgeColorDirect <-
    function (edge.names,
              new.value,
              network = NULL,
              base.url = .defaultBaseUrl) {
        for (color in new.value) {
            if (.isNotHexColor(color)) {
                return()
            }
        }
        setEdgePropertyDirect(edge.names,
                              new.value,
                              "EDGE_STROKE_UNSELECTED_PAINT",
                              network,
                              base.url)
        setEdgePropertyDirect(edge.names,
                              new.value,
                              "EDGE_UNSELECTED_PAINT",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLabelDirect()
#' }
#' @export
setEdgeLabelDirect <-
    function(edge.names,
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyDirect(edge.names, new.value, "EDGE_LABEL", network, base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Face Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeFontFaceDirect()
#' }
#' @export
setEdgeFontFaceDirect <-
    function(edge.names,
             new.value,
             network = NULL,
             base.url = .defaultBaseUrl) {
        setEdgePropertyDirect(edge.names,
                              new.value,
                              "EDGE_LABEL_FONT_FACE",
                              network,
                              base.url)
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeFontSizeDirect()
#' }
#' @export
setEdgeFontSizeDirect <-
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
                        'illegal font string "%s" in RCy3::setEdgeFontSizeDirect():\t\n it needs to be a valid number.',
                        current.size
                    ),
                    stderr ()
                )
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if (size.type.errors < 1) {
            setEdgePropertyDirect(edge.names,
                                  new.value,
                                  "EDGE_LABEL_FONT_SIZE",
                                  network,
                                  base.url)
        }
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLabelColorDirect()
#' }
#' @export
setEdgeLabelColorDirect <-
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
        # set the edge property direct
        return(
            setEdgePropertyDirect(
                edge.names,
                new.value,
                "EDGE_LABEL_COLOR",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Tooltip Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTooltipDirect()
#' }
#' @export
setEdgeTooltipDirect <-
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
        # set the edge property direct
        return(
            setEdgePropertyDirect(edge.names, new.values, "EDGE_TOOLTIP", network, base.url)
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLineWidthDirect()
#' }
#' @export
setEdgeLineWidthDirect <-
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
        
        # set the edge property direct
        return(setEdgePropertyDirect(edge.names, new.value, "EDGE_WIDTH", network, base.url))
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLineStyleDirect()
#' }
#' @export
setEdgeLineStyleDirect <-
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
                    "\n\t\tERROR in setEdgeLineStyleDirect() >> INVALID line
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
            setEdgePropertyDirect(
                edge.names,
                toupper(new.values),
                "EDGE_LINE_TYPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Shape Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeSourceArrowShapeDirect()
#' }
#' @export
setEdgeSourceArrowShapeDirect <-
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
                    "\n\t\tERROR in setEdgeSourceArrowShapeDirect() >>
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
            setEdgePropertyDirect(
                edge.names,
                toupper(new.values),
                "EDGE_SOURCE_ARROW_SHAPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Shape Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.values DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTargetArrowShapeDirect()
#' }
#' @export
setEdgeTargetArrowShapeDirect <-
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
                    "\n\t\tERROR in setEdgeTargetArrowShapeDirect() >> INVALID arrow shape value(s): ",
                    error.msg,
                    "\n",
                    sep = ""
                )
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(
            setEdgePropertyDirect(
                edge.names,
                toupper(new.values),
                "EDGE_TARGET_ARROW_SHAPE",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.colors DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeSourceArrowColorDirect()
#' }
#' @export
setEdgeSourceArrowColorDirect <-
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
            setEdgePropertyDirect(
                edge.names,
                new.colors,
                "EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.colors DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTargetArrowColorDirect()
#' }
#' @export
setEdgeTargetArrowColorDirect <-
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
            setEdgePropertyDirect(
                edge.names,
                new.colors,
                "EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                network,
                base.url
            )
        )
    }

# ------------------------------------------------------------------------------
#' @title Set Edge Label Opacity Direct
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.names DESCRIPTION
#' @param new.value DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLabelOpacityDirect()
#' }
#' @export
setEdgeLabelOpacityDirect <-
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
                        "\n\t\tERROR in setEdgeLabelOpacityDirect(): illegal opacity
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
            setEdgePropertyDirect(
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
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' hideSelectedEdges()
#' }
#' @export
hideSelectedEdges <- function (base.url = .defaultBaseUrl) {
    edge.names <- getSelectedEdges(base.url)
    setEdgePropertyDirect(edge.names, 'false', "EDGE_VISIBLE", network, base.url)
} 

# ------------------------------------------------------------------------------
#' @title Unhide All
#'
#' @description FUNCTION_DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' unhideAll()
#' }
#' @export
unhideAll <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    node.names <- getAllNodes(suid, base.url)
    setNodePropertyDirect(node.names, 'true', "NODE_VISIBLE", network, base.url)
    
    edge.names <- getAllEdges(suid, base.url)
    setEdgePropertyDirect(edge.names, 'true', "EDGE_VISIBLE", network, base.url)
}
