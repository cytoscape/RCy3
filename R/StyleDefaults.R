
# ------------------------------------------------------------------------------
#' Updates the default values of visual properties in a style
#'
#' @description Updates visual property defaults, overriding any prior settings. See mapVisualProperty for
#' the list of visual properties.
#' @param style.name (char) name for style
#' @param defaults (list) a list of visual property default settings, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
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
getDefaultVisualProperty <- function(property, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles", as.character(style.name), "defaults", property, sep="/"), base.url=base.url)
    return(res[[2]])
}

# ------------------------------------------------------------------------------
setDefaultVisualProperty <- function(style.string, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestPUT(paste("styles", as.character(style.name), "defaults", sep="/"),
                     body = list(style.string),
                     base.url=base.url)
    invisible(res)
}


#========================================================================================================================
# Individual Properties
#==========================



# ------------------------------------------------------------------------------
#' @export
setDefaultNodeShape <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    new.shape <- toupper(new.shape)
    if (new.shape %in% getNodeShapes(base.url)){
        style = list(visualProperty = "NODE_SHAPE", value = new.shape)
        setDefaultVisualProperty(style, style.name, base.url)
    }else{
        write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
    }
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeSize <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    lockNodeDimensions(TRUE, style.name, base.url)
    
    style <- list(visualProperty = "NODE_SIZE", value = new.size)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeBorderColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeBorderWidth <-  function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeFontSize <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeLabelColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultEdgeLineWidth <- function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultEdgeLineStyle <- function(new.line.style, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
setDefaultEdgeColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}

#' @export
setDefaultEdgeSourceArrowColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}

#' @export
setDefaultEdgeTargetArrowColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}
# ------------------------------------------------------------------------------
#' @export
setDefaultEdgeFontSize <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
getDefaultBackgroundColor <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getDefaultVisualProperty('NETWORK_BACKGROUND_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setDefaultBackgroundColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NETWORK_BACKGROUND_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
getDefaultNodeSelectionColor <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getDefaultVisualProperty('NODE_SELECTED_PAINT', style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setDefaultNodeSelectionColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}
#------------------------------------------------------------------------------------------------------------------------
#' @export
getDefaultNodeReverseSelectionColor <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getDefaultVisualProperty('NODE_PAINT', style.name, base.url))
}
#------------------------------------------------------------------------------------------------------------------------
#' @export
setDefaultNodeReverseSelectionColor <- function (new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @export
getDefaultEdgeSelectionColor <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getDefaultVisualProperty('EDGE_STROKE_SELECTED_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @export
setDefaultEdgeSelectionColor <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getDefaultEdgeReverseSelectionColor <- function (style.name='default', base.url=.defaultBaseUrl) {
    return(getDefaultVisualProperty('EDGE_STROKE_UNSELECTED_PAINT',style.name, base.url))
}
#------------------------------------------------------------------------------------------------------------------------
#' @export
setDefaultEdgeReverseSelectionColor <- function (new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setDefaultVisualProperty(style, style.name, base.url)
}
