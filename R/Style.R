# ------------------------------------------------------------------------------
#' @export
copyVisualStyle <- function (from.style, to.style, base.url=.defaultBaseUrl) {
    current.names = getVisualStyleNames(base.url)
    if (! from.style %in% current.names){
        stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
    }
    # get the current style from Cytoscape
    res <- cyrestGET(paste("styles", from.style, sep="/"), base.url = base.url)
    style.from.to <- res
    style.from.to[1] <- as.character(to.style)
    
    # and send it to Cytoscape as a new style with a new name
    cyrestPOST("styles", body = style.from.to, base.url = base.url)
    
    # get and update dependencies as well
    res <- cyrestGET(paste("styles", from.style, "dependencies", sep="/"), base.url = base.url)
    cyrestPUT(paste("styles", to.style, "dependencies", sep="/"), body=res, base.url = base.url)
}


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

createVisualStyle <- function(style.name, defaults, mappings, base.url=.defaultBaseUrl) {
    
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
#' @export
getArrowShapes <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/"), base.url = base.url)
    return(res$values)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getLineStyles <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/EDGE_LINE_TYPE/values", sep="/"), base.url = base.url)
    return(res$values)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getNodeShapes <- function (base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/NODE_SHAPE/values", sep="/"),base.url=base.url)
    return(res$values)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getVisualPropertyNames <- function (base.url=.defaultBaseUrl) {
    res <- cyrestGET("styles/default/defaults", base.url = base.url)
    visual.properties <- unname(res[[1]])
    visual.properties <- sapply(visual.properties, '[[', 1)
    return(visual.properties)
}

# ------------------------------------------------------------------------------
#' @export
getVisualStyleNames <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET("apply/styles",base.url = base.url)
    return(unname(res))
}

# ------------------------------------------------------------------------------
#' Save Visual Styles
#'
#' @description Saves one or more visual styles to file
#' @param filename (char) name of the style file to save. Default is "styles.xml"
#' @param type (char) type of data file to export, e.g., XML, JSON (case sensitive). Default is XML.
#' @param styles (optional) The styles to be exported, listed as a comma-separated string. 
#' If no styles are specified, only the current one is exported.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' saveVisualStyles()
#' saveVisualStyles('myStyle', type = 'JSON')
#' saveVisualStyles('myStyle', style = 'Minimal,default,Directed')
#' }

saveVisualStyles<-function(filename=NULL, type=NULL, styles=NULL, base.url=.defaultBaseUrl){
    cmd.string <- 'vizmap export'  # minmum command
    if(!is.null(filename))
        cmd.string <- paste0(cmd.string,' OutputFile="',filename,'"')
    if(!is.null(type))
        cmd.string <- paste0(cmd.string,' options="',type,'"')
    if(!is.null(styles))
        cmd.string <- paste0(cmd.string,' styles="',styles,'"')
    commandsPOST(cmd.string, base.url = base.url)
}

# ------------------------------------------------------------------------------
# apply visual style to network
#' @export
setVisualStyle <- function(style.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    current.names = getVisualStyleNames(base.url)
    # inform user if they want to set style that does not exist 
    if(!style.name %in% current.names) { 
        stop(sprintf('Cannot call setVisualStyle on a non-existent visual style (%s)', style.name))
    }
    cyrestGET(paste("apply/styles", style.name, net.SUID, sep="/"), base.url = base.url)
    write(sprintf('network visual style has been set to "%s"', style.name), stdout())
}



