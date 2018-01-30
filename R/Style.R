#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

#-----------------------------------------------------------
# methods related to visual styles
#-----------------------------------------------------------
setGeneric ('setVisualStyle',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), style.name) standardGeneric ('setVisualStyle'))
setGeneric ('redraw',				signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('redraw'))
setGeneric ('getVisualStyleNames',  signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getVisualStyleNames'))
setGeneric ('copyVisualStyle',      signature='obj', function (obj=CytoscapeConnection(), from.style, to.style) standardGeneric ('copyVisualStyle'))
setGeneric ('lockNodeDimensions',   signature='obj', function (obj=CytoscapeConnection(), new.state, style.name='default') standardGeneric ('lockNodeDimensions'))
setGeneric ('getLineStyles',        signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLineStyles'))
setGeneric ('getArrowShapes', 	    signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getArrowShapes'))
setGeneric ('getNodeShapes',        signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNodeShapes'))
setGeneric ('getDirectlyModifiableVisualProperties',    
                                    signature='obj', function(obj=CytoscapeConnection(), style.name="default") standardGeneric ('getDirectlyModifiableVisualProperties'))

# ------------------------------------------------------------------------------
#' Create a visual style from components
#'
#' @description Creates a style from defaults and predefined mappings.
#' @details Requires attribute mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param defaults (list) key-value pairs for default mappings.
#' @param mappings (list) visual property mappings, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
#' @return None
#' @export
#' @import RJSONIO
#' @import httr
#' @examples
#' \donttest{
#' #first there has to be a network to apply style to
#' example(createNetworkFromDataFrames)
#'
#' #then prepare style variables
#' style.name = "myStyle"
#' defaults <- list(NODE_SHAPE="diamond",
#'                  NODE_SIZE=30,
#'                  EDGE_TRANSPARENCY=120,
#'                  NODE_LABEL_POSITION="W,E,c,0.00,0.00")
#' nodeLabels <- mapVisualProperty('node label','id','p')
#' nodeFills <- mapVisualProperty('node fill color','group','d',c("A","B"), c("#FF9900","#66AAAA"))
#' arrowShapes <- mapVisualProperty('Edge Target Arrow Shape','interaction','d',
#'                                  c("activates","inhibits","interacts"),c("Arrow","T","None"))
#' edgeWidth <- mapVisualProperty('edge width','weight','p')
#'
#' #and then create the style
#' createVisualStyle(style.name, defaults, list(nodeLabels,nodeFills,arrowShapes,edgeWidth))
#'
#' #finsh by applying the style
#' example(applyStyle)
#' }
#' @seealso applyStyle, mapVisualProperty

createVisualStyle <- function(style.name, defaults, mappings, obj=CytoscapeConnection()) {
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    if(missing(mappings))
        mappings <- list()
    
    styleDef <- list()
    if(!missing(defaults)){
        for (i in 1:length(defaults)) {
            styleDef[[i]] <- list(visualProperty=names(defaults)[i], value=defaults[[i]])
        }
    }
    style <- list(title=style.name, defaults=styleDef,mappings=mappings)
    jsonStyle <- toJSON(style)
    style.url <- paste(base.url,'styles', sep = '/')
    invisible(POST(url=style.url,body=jsonStyle, encode="json"))
}

# ------------------------------------------------------------------------------
setMethod('redraw', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              resource.uri <- paste(obj@uri, obj@api, "apply/styles", "default", net.SUID, sep = "/")
              request.res <- GET(url=resource.uri)
              invisible(request.res)
          }) 
## END redraw

# ------------------------------------------------------------------------------
#' Saves the current visual style as a data file
#'
#' @param filename (char) name of the style file to save
#' @param type (char) type of data file to export, e.g., XML, JSON (case sensitive)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' saveVisualStyle('myStyle','JSON')
#' }

saveVisualStyle<-function(filename,type,obj=CytoscapeConnection()){
    commandRun(paste0('vizmap export options=',type,' OutputFile="',filename,'"'),obj)
}


# ------------------------------------------------------------------------------
#' Updates the values of dependencies in a style
#'
#' @description Updates style dependencies, overriding any prior settings.
#' @param style.name (char) name for style
#' @param dependencies (list) a list of style dependencies, see list below. Note:
#' each dependency is set by a boolean, TRUE or FALSE (T or F)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDependencies("myStyle",list(nodeSizeLocked=TRUE))
#' }
#' @export
#' @section List of Dependencies:
#' arrowColorMatchesEdge
#' nodeCustomGraphicsSizeSync
#' nodeSizeLocked

updateStyleDependencies <- function(style.name,dependencies,obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    dep.list <- list()
    for (i in 1:length(dependencies)) {
        dep.list[[i]] <- list(visualPropertyDependency=names(dependencies)[i],
                              enabled=dependencies[[i]])
    }
    
    style.url <- URLencode(paste(base.url,'styles', style.name,'dependencies', sep = '/'))
    map.body <- toJSON(dep.list)
    
    cat("PUT-ing style\n")
    invisible(PUT(url=style.url,body=map.body, encode="json"))
    invisible(commandRun(paste('vizmap apply styles',style.name,sep='=')))
}


#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'OptionalCyObjClass', function (obj) {
    resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/NODE_SHAPE/values", sep="/")
    request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDirectlyModifiableVisualProperties', 'OptionalCyObjClass',
           function (obj, style.name="default") {
               resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
               request.res = GET(url=resource.uri)
               visual.properties <- unname(fromJSON(rawToChar(request.res$content))[[1]])
               visual.properties <- sapply(visual.properties, '[[', 1)
               return(visual.properties)
           })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'OptionalCyObjClass', function (obj) {
    resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/EDGE_LINE_TYPE/values", sep="/")
    request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
})

# ------------------------------------------------------------------------------
setMethod('getArrowShapes', 'OptionalCyObjClass', 
          function(obj) {
              
              
              resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/")
              # TanjaM: EDGE_SOURCE_ARROW_SHAPE rather than TARGET returns the same results as of April 2015
              request.res <- GET(resource.uri)
              request.res <- fromJSON(rawToChar(request.res$content))
              return(request.res$values)
          })
## END getArrowShapes

# ------------------------------------------------------------------------------
setMethod('getVisualStyleNames', 'OptionalCyObjClass', 
          function(obj) {
              resource.uri = paste(obj@uri, obj@api, "apply/styles", sep="/")
              request.res = GET(url=resource.uri)
              style.names = unname(fromJSON(rawToChar(request.res$content)))
              return(style.names)
          })

# ------------------------------------------------------------------------------
setMethod('copyVisualStyle', 'OptionalCyObjClass', 
          function (obj, from.style, to.style) {
              current.names = getVisualStyleNames (obj)
              if (! from.style %in% current.names){
                  stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
              }
              # get the current style from Cytoscape
              resource.uri <- paste(obj@uri, obj@api, "styles", from.style, sep="/")
              from.style.JSON <- GET(url=URLencode(resource.uri))
              from.style <- fromJSON(rawToChar(from.style.JSON$content))
              from.style[1] <- as.character(to.style)
              
              # and send it to Cytoscape as a new style with a new name
              to.style.JSON <- toJSON(from.style)
              resource.uri <- paste(obj@uri, obj@api, "styles", sep="/")
              request.res <- POST(url = resource.uri, body = to.style.JSON, encode = "json")
              invisible(request.res)
          })

# ------------------------------------------------------------------------------
# apply visual style to network
setMethod('setVisualStyle', 'OptionalCyWinClass', 
          function(obj, style.name) {
              
              net.SUID = as.character(obj@suid)
              current.names = getVisualStyleNames(obj)
              # inform user if they want to set style that does not exist 
              if(!style.name %in% current.names) { 
                  stop(sprintf('Cannot call setVisualStyle on a non-existent visual style (%s)', style.name))
              }
              # apply style
              resource.uri <- paste(obj@uri, obj@api, "apply/styles", style.name, net.SUID, sep="/")
              req.res <- GET(url=URLencode(resource.uri))
              write(sprintf('network visual style has been set to "%s"', style.name), stdout())
              invisible(req.res)
          })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('lockNodeDimensions', 'OptionalCyObjClass',
           
           function (obj, new.state, style.name='default') {
               # launch error if visual style name is missing
               if (! style.name %in% getVisualStyleNames (obj)) {
                   write (sprintf ('Error in RCy3::lockNodeDimensions. No visual style named "%s"', style.name), stdout ())
                   return ()
               }
               
               #lock node dimensions
               resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "dependencies", sep="/")
               style <- list(visualPropertyDependency="nodeSizeLocked", enabled = tolower(new.state))
               style.JSON <- toJSON(list(style))
               request.res <- PUT(url=resource.uri, body=style.JSON, encode="json")
               
               # inform the user if the request was a success or failure
               if (request.res$status == 204){
                   if (new.state==TRUE){
                       write (sprintf ('Locked node dimensions successfully even if the check box is not ticked.'), stdout ())
                   }else{
                       write (sprintf ('Unlocked node dimensions successfully even if the check box is not ticked.'), stdout ())
                   }
               }else{
                   write (sprintf ('Error in RCy3::lockNodeDimensions. Could not lock/unlocked node dimensions'), stderr ())
               }
               invisible(request.res)
               
           }) # lockNodeDimensions


