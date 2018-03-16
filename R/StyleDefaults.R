# ==============================================================================
# Functions for getting and setting DEFAULT values for visual properties,
# organized into sections:
#
# I. General functions for setting node, edge and network defaults
# II. Specific functions for setting particular node, edge and network defaults
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' Updates the default values of visual properties in a style
#'
#' @description Updates visual property defaults, overriding any prior settings. See mapVisualProperty for
#' the list of visual properties.
#' @param style.name (char) name for style
#' @param defaults (list) a list of visual property default settings
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDefaults('myStyle',list('node fill color'='#0000FF','node size'=50))
#' }
#' @export
#' @seealso mapVisualProperty
updateStyleDefaults <- function(style.name,defaults,base.url=.defaultBaseUrl){
    
    def.list <- list()
    for (i in 1:length(defaults)) {
        visual.prop.name <- names(defaults)[i]
        visual.prop.name = toupper(gsub("\\s+","_",visual.prop.name))
        visual.prop.name = switch(visual.prop.name,
                                  'EDGE_COLOR'='EDGE_STROKE_UNSELECTED_PAINT',
                                  'EDGE_THICKNESS'='EDGE_WIDTH',
                                  'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                                  visual.prop.name)
        def.list[[i]] <- list(visualProperty=visual.prop.name,
                              value=defaults[[i]])
    }
    invisible(cyrestPUT(paste('styles', style.name,'defaults', sep = '/'),
                        body=def.list, base.url = base.url))
}

# ------------------------------------------------------------------------------
#' @title Get Visual Property Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param property Name of property, e.g., NODE_FILL_COLOR (see \link{getVisualPropertyNames})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getVisualPropertyDefault()
#' }
#' @export
getVisualPropertyDefault <- function(property, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles", as.character(style.name), "defaults", property, sep="/"), base.url=base.url)
    return(res[[2]])
}

# ------------------------------------------------------------------------------
#' @title Set Visual Property Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.string A named list including "visualProperty" and "value"
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setVisualPropertyDefault()
#' }
#' @export
setVisualPropertyDefault <- function(style.string, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestPUT(paste("styles", as.character(style.name), "defaults", sep="/"),
                     body = list(style.string),
                     base.url=base.url)
    invisible(res)
}

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. Node Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Set Node Shape Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.shape Name of shape, e.g., ARROW, T, etc (see \link{getArrowShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeShapeDefault()
#' }
#' @export
setNodeShapeDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    new.shape <- toupper(new.shape)
    if (new.shape %in% getNodeShapes(base.url)){
        style = list(visualProperty = "NODE_SHAPE", value = new.shape)
        setVisualPropertyDefault(style, style.name, base.url)
    }else{
        write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
    }
}

# ------------------------------------------------------------------------------
#' @title Set Node Size Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeSizeDefault()
#' }
#' @export
setNodeSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    lockNodeDimensions(TRUE, style.name, base.url)
    
    style <- list(visualProperty = "NODE_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeColorDefault()
#' }
#' @export
setNodeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderColorDefault()
#' }
#' @export
setNodeBorderColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.width Numeric value for width
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderWidthDefault()
#' }
#' @export
setNodeBorderWidthDefault <-  function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeFontSizeDefault()
#' }
#' @export
setNodeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Label Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelColorDefault()
#' }
#' @export
setNodeLabelColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getNodeSelectionColorDefault()
#' }
#' @export
getNodeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NODE_SELECTED_PAINT', style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Node Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeSelectionColorDefault()
#' }
#' @export
setNodeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Reverse Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getNodeReverseSelectionColorDefault()
#' }
#' @export
getNodeReverseSelectionColorDefault <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NODE_PAINT', style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Node Reverse Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeReverseSelectionColorDefault()
#' }
#' @export
setNodeReverseSelectionColorDefault <- function (new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ==============================================================================
# II.b. Edge Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.width Numeric value for width
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineWidthDefault()
#' }
#' @export
setEdgeLineWidthDefault <- function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.line.style  Name of line style, e.g., SOLID, LONG_DASH, etc (see \link{getLineStyles})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineStyleDefault()
#' }
#' @export
setEdgeLineStyleDefault <- function(new.line.style, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeColorDefault()
#' }
#' @export
setEdgeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowColorDefault()
#' }
#' @export
setEdgeSourceArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowColorDefault()
#' }
#' @export
setEdgeTargetArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.shape Name of shape, e.g., ARROW, T, etc (see \link{getArrowShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowDefault()
#' }
#' @export
setEdgeSourceArrowDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_SOURCE_ARROW_SHAPE", value = new.shape) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.shape Name of shape, e.g., ARROW, T, etc (see \link{getArrowShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowDefault()
#' }
#' @export
setEdgeTargetArrowDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_TARGET_ARROW_SHAPE", value = new.shape) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeFontSizeDefault()
#' }
#' @export
setEdgeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getEdgeSelectionColorDefault()
#' }
#' @export
getEdgeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('EDGE_STROKE_SELECTED_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Edge Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSelectionColorDefault()
#' }
#' @export
setEdgeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Reverse Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getEdgeReverseSelectionColorDefault()
#' }
#' @export
getEdgeReverseSelectionColorDefault <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('EDGE_STROKE_UNSELECTED_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Edge Reverse Selection Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeReverseSelectionColorDefault()
#' }
#' @export
setEdgeReverseSelectionColorDefault <- function (new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ==============================================================================
# II.c. Network Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Get Background Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getBackgroundColorDefault()
#' }
#' @export
getBackgroundColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NETWORK_BACKGROUND_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Background Color Default
#'
#' @description FUNCTION_DESCRIPTION
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setBackgroundColorDefault()
#' }
#' @export
setBackgroundColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NETWORK_BACKGROUND_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

