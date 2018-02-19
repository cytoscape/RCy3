# ==============================================================================
# Functions for managing STYLES and retrieving general lists of properties
# relevant to multiple style modes. Functions specific to Default, Mapping,
# Bypass, Dependencies and Values are in separate files.
# 
# I. Style management functions 
# II. General property functions 
#
# ==============================================================================
# I. Style management functions
# ------------------------------------------------------------------------------
#' @title Copy Visual Style
#'
#' @description FUNCTION_DESCRIPTION
#' @param from.style DESCRIPTION
#' @param to.style DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' copyVisualStyle()
#' }
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
#' Export Visual Styles
#'
#' @description Saves one or more visual styles to file
#' @param filename (char) Name of the style file to save. Default is "styles.xml"
#' @param type (optional) Type of data file to export, e.g., XML, JSON (case sensitive). 
#' Default is XML.
#' @param styles (optional) The styles to be exported, listed as a comma-separated string. 
#' If no styles are specified, only the current one is exported.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return None
#' @examples
#' \donttest{
#' exportVisualStyles('/fullpath/myStyle')
#' exportVisualStyles('/fullpath/myStyle', type = 'JSON')
#' exportVisualStyles('/fullpath/myStyle', style = 'Minimal,default,Directed')
#' }
#' @export
exportVisualStyles<-function(filename=NULL, type=NULL, styles=NULL, base.url=.defaultBaseUrl){
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
#' @title Get Visual Style Names
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getVisualStyleNames()
#' }
#' @export
getVisualStyleNames <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET("apply/styles",base.url = base.url)
    return(unname(res))
}

# ------------------------------------------------------------------------------
#' @title Set Visual Style
#'
#' @description Apply a visual style to a network
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setVisualStyle()
#' }
#' @export
setVisualStyle <- function(style.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    current.names = getVisualStyleNames(base.url)
    # inform user if they want to set style that does not exist 
    if(!style.name %in% current.names) { 
        stop(sprintf('Cannot call setVisualStyle on a non-existent visual style (%s)', style.name))
    }
    cyrestGET(paste("apply/styles", style.name, net.SUID, sep="/"), base.url = base.url)
}

# ==============================================================================
# II. General property functions 
# ------------------------------------------------------------------------------
#' @title Get Arrow Shapes
#'
#' @description Retrieve the names of the currently supported 'arrows' -- the
#' decorations can (optionally) appear at the ends of edges, adjacent to
#' the nodes they connect, and conveying information about the nature of
#' the nodes' relationship.
#' @param base.url DESCRIPTION
#' @return A list of \code{character} strings, e.g., 'DIAMOND', 'T', 'ARROW'
#' @examples \donttest{
#' getArrowShapes()
#' }
#' @export
getArrowShapes <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/"), base.url = base.url)
    return(res$values)
}

# ------------------------------------------------------------------------------
#' @title Get Line Styles
#'
#' @description Retrieve the names of the currently supported line types
#' -- values which can be used to render edges, and thus
#' can be used in calls to 'setEdgeLineStyleRule'.
#' @param base.url DESCRIPTION
#' @return A list of \code{character} strings, e.g., 'SOLID', 'DOT'
#' @examples \donttest{
#' getLineStyles()
#' }
#' @export
getLineStyles <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/EDGE_LINE_TYPE/values", sep="/"), base.url = base.url)
    return(res$values)
}

# ------------------------------------------------------------------------------
#' @title Get Node Shapes
#'
#' @description Retrieve the names of the currently supported node shapes,
#' which can then be used in calls to setNodeShapeRule and
#' setDefaultVizMapValue.
#' @param base.url DESCRIPTION
#' @return A list of \code{character} strings, e.g. 'ELLIPSE', 'RECTANGLE'
#' @examples \donttest{
#' getNodeShapes()
#' }
#' @export
getNodeShapes <- function (base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles/visualproperties/NODE_SHAPE/values", sep="/"),base.url=base.url)
    return(res$values)
}

# ------------------------------------------------------------------------------
#' @title Get Visual Property Names
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getVisualPropertyNames()
#' }
#' @export
getVisualPropertyNames <- function (base.url=.defaultBaseUrl) {
    res <- cyrestGET("styles/default/defaults", base.url = base.url)
    visual.properties <- unname(res[[1]])
    visual.properties <- sapply(visual.properties, '[[', 1)
    return(visual.properties)
}



