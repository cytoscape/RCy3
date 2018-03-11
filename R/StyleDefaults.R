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
    invisible(cyrestPUT(url=paste('styles', style.name,'defaults', sep = '/'),
                        body=def.list, base.url = base.url))
}

# ------------------------------------------------------------------------------
#' @export
getVisualPropertyDefault <- function(property, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles", as.character(style.name), "defaults", property, sep="/"), base.url=base.url)
    return(res[[2]])
}

# ------------------------------------------------------------------------------
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
#' @export
setNodeSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    lockNodeDimensions(TRUE, style.name, base.url)
    
    style <- list(visualProperty = "NODE_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setNodeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setNodeBorderColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setNodeBorderWidthDefault <-  function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setNodeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setNodeLabelColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
getNodeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NODE_SELECTED_PAINT', style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setNodeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}
#------------------------------------------------------------------------------------------------------------------------
#' @export
getNodeReverseSelectionColorDefault <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NODE_PAINT', style.name, base.url))
}
#------------------------------------------------------------------------------------------------------------------------
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
#' @export
setEdgeLineWidthDefault <- function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setEdgeLineStyleDefault <- function(new.line.style, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setEdgeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

#' @export
setEdgeSourceArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

#' @export
setEdgeTargetArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}
# ------------------------------------------------------------------------------
#' @export
setEdgeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
getEdgeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('EDGE_STROKE_SELECTED_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setEdgeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getEdgeReverseSelectionColorDefault <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('EDGE_STROKE_UNSELECTED_PAINT',style.name, base.url))
}
#------------------------------------------------------------------------------------------------------------------------
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
#' @export
getBackgroundColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NETWORK_BACKGROUND_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setBackgroundColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NETWORK_BACKGROUND_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

