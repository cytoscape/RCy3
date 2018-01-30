#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

#--- defaults ----------------------------------------------
setGeneric ('getDefaultBackgroundColor',	        signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultBackgroundColor'))
setGeneric ('setDefaultBackgroundColor',            signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultBackgroundColor'))
setGeneric ('getDefaultNodeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultNodeSelectionColor'))
setGeneric ('setDefaultNodeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeSelectionColor'))
setGeneric ('getDefaultNodeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultNodeReverseSelectionColor'))
setGeneric ('setDefaultNodeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeReverseSelectionColor'))
setGeneric ('getDefaultEdgeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultEdgeSelectionColor'))
setGeneric ('setDefaultEdgeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), new.color,  style.name='default') standardGeneric ('setDefaultEdgeSelectionColor'))
setGeneric ('getDefaultEdgeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultEdgeReverseSelectionColor'))
setGeneric ('setDefaultEdgeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeReverseSelectionColor'))

setGeneric ('setDefaultNodeShape',        signature='obj', function (obj=CytoscapeConnection(), new.shape, style.name='default') standardGeneric ('setDefaultNodeShape'))
setGeneric ('setDefaultNodeSize',         signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultNodeSize'))
setGeneric ('setDefaultNodeColor',        signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeColor'))
setGeneric ('setDefaultNodeBorderColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeBorderColor'))
setGeneric ('setDefaultNodeBorderWidth',  signature='obj', function (obj=CytoscapeConnection(), new.width, style.name='default') standardGeneric ('setDefaultNodeBorderWidth'))
setGeneric ('setDefaultNodeFontSize',     signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultNodeFontSize'))
setGeneric ('setDefaultNodeLabelColor',   signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeLabelColor'))

setGeneric ('setDefaultEdgeLineWidth',        signature='obj', function (obj=CytoscapeConnection(), new.width, style.name='default') standardGeneric ('setDefaultEdgeLineWidth'))
setGeneric ('setDefaultEdgeLineStyle',        signature='obj', function (obj=CytoscapeConnection(), new.line.style, style.name='default') standardGeneric ('setDefaultEdgeLineStyle'))
setGeneric ('setDefaultEdgeColor',            signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeColor'))
setGeneric ('setDefaultEdgeSourceArrowColor', signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeSourceArrowColor'))
setGeneric ('setDefaultEdgeTargetArrowColor', signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeTargetArrowColor'))
setGeneric ('setDefaultEdgeFontSize',         signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultEdgeFontSize'))

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

updateStyleDefaults <- function(style.name,defaults,obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
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
    
    style.url <- URLencode(paste(base.url,'styles', style.name,'defaults', sep = '/'))
    map.body <- toJSON(def.list)
    invisible(PUT(url=style.url,body=map.body, encode="json"))
    
}

# ------------------------------------------------------------------------------
getVisualProperty <- function(obj, style.name, property) {
    resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", property, sep="/")
    request.res <- GET(url=resource.uri)
    return(fromJSON(rawToChar(request.res$content))[[2]])
}

# ------------------------------------------------------------------------------
setVisualProperty <- function(obj, style.string, style.name='default') {
    resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
    style.JSON <- toJSON(list(style.string))
    request.res <- PUT(url=resource.uri, body=style.JSON, encode="json")
    invisible(request.res)
}


#========================================================================================================================
# Individual Properties
#==========================


# ------------------------------------------------------------------------------
setMethod('setDefaultNodeShape', 'OptionalCyObjClass', 
          function(obj, new.shape, style.name='default') {
              new.shape <- toupper(new.shape)
              if (new.shape %in% getNodeShapes(obj)){
                  style = list(visualProperty = "NODE_SHAPE", value = new.shape)
                  setVisualProperty(obj, style, style.name)
              }else{
                  write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
              }
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSize', 'OptionalCyObjClass', 
          function(obj, new.size, style.name='default') {
              # lock node dimensions
              lockNodeDimensions (obj, TRUE)
              
              style <- list(visualProperty = "NODE_SIZE", value = new.size)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeBorderColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderWidth', 'OptionalCyObjClass', 
           function(obj, new.width, style.name='default') {
               style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
               setVisualProperty(obj, style, style.name)
           })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeFontSize', 'OptionalCyObjClass', 
          function(obj, new.size, style.name='default') {
              style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeLabelColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }      
              style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeLineWidth', 'OptionalCyObjClass', 
          function(obj, new.width, style.name='default') {
              style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeLineStyle', 'OptionalCyObjClass', 
          function(obj, new.line.style, style.name='default') {
              style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              # TODO Comment Tanja: maybe change to EDGE_UNSELECTED_PAINT
              style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })

setMethod('setDefaultEdgeSourceArrowColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })

setMethod('setDefaultEdgeTargetArrowColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })
# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeFontSize', 'OptionalCyObjClass', 
          function(obj, new.size, style.name='default') {
              style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('getDefaultBackgroundColor', 'OptionalCyObjClass', 
          function(obj, style.name='default') {
              resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults/NETWORK_BACKGROUND_PAINT", sep="/")
              request.res = GET(url=resource.uri)
              def.background.color = fromJSON(rawToChar(request.res$content))[[2]]
              return(def.background.color)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultBackgroundColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              } 
              resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
              style = list(visualProperty = 'NETWORK_BACKGROUND_PAINT', value = new.color)
              style.JSON = toJSON(list(style))
              request.res = PUT(url=resource.uri, body=style.JSON, encode="json")
              invisible(request.res)
          })

# ------------------------------------------------------------------------------
setMethod('getDefaultNodeSelectionColor', 'OptionalCyObjClass', 
          function(obj, style.name='default') {
              return(getVisualProperty(obj, style.name, 'NODE_SELECTED_PAINT'))
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSelectionColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              } 
              style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeReverseSelectionColor',  'OptionalCyObjClass',
           
           function (obj, style.name='default') {
               return(getVisualProperty(obj, style.name, 'NODE_PAINT'))
           })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeReverseSelectionColor',  'OptionalCyObjClass',
           
           function (obj, new.color, style.name='default') {
               if (.isNotHexColor(new.color)){
                   return()
               } 
               style = list(visualProperty = "NODE_PAINT", value = new.color) 
               setVisualProperty(obj, style, style.name)
           })

# ------------------------------------------------------------------------------
setMethod('getDefaultEdgeSelectionColor', 'OptionalCyObjClass', 
          function(obj, style.name='default') {
              return(getVisualProperty(obj, style.name, 'EDGE_STROKE_SELECTED_PAINT'))
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeSelectionColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'OptionalCyObjClass',
           
           function (obj, style.name='default') {
               return(getVisualProperty(obj, style.name, 'EDGE_STROKE_UNSELECTED_PAINT'))
           })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'OptionalCyObjClass',
           
           function (obj, new.color, style.name='default') {
               if (.isNotHexColor(new.color)){
                   return()
               } 
               style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
               setVisualProperty(obj, style, style.name)
           })
