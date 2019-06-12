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
#' @description Create a new visual style by copying a specified style.
#' @param from.style Name of visual style to copy
#' @param to.style Name of new visual style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
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
#' @description Create a style from defaults and predefined mappings.
#' @details Requires attribute mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param defaults (list) key-value pairs for default mappings.
#' @param mappings (list) visual property mappings, see mapVisualProperty
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
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
#' setVisualStyle(style.name)
#' }
#' @seealso applyStyle, mapVisualProperty
createVisualStyle <- function(style.name, defaults, mappings, base.url=.defaultBaseUrl) {
    
    if(missing(mappings))
        mappings <- list()
    
    styleDef <- list()
    if(!missing(defaults)){
        for (i in seq_len(length(defaults))) {
            styleDef[[i]] <- list(visualProperty=names(defaults)[i], value=defaults[[i]])
        }
    }
    style <- list(title=style.name, defaults=styleDef,mappings=mappings)
    jsonStyle <- toJSON(style)
    style.url <- paste(base.url,'styles', sep = '/')
    invisible(POST(url=style.url,body=jsonStyle, encode="json"))
}

# ------------------------------------------------------------------------------
#' @title Delete Visual Style
#'
#' @description Deletes the specified visual style from current session.
#' @param style.name (char) name of style to delete
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' deleteVisualStyle("myStyle")
#' }
#' @export
deleteVisualStyle<-function(style.name, base.url=.defaultBaseUrl){
    cyrestDELETE(paste('styles',style.name, sep='/'), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export Visual Styles
#'
#' @description Save one or more visual styles to file.
#' @param filename (char) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. Default is 
#' "styles.xml"
#' @param type (optional) Type of data file to export, e.g., XML, JSON (case 
#' sensitive). 
#' Default is XML. Note: Only XML can be read by importVisualStyles().
#' @param styles (optional) The styles to be exported, listed as a 
#' comma-separated string. 
#' If no styles are specified, only the current one is exported.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Path to saved file
#' @examples
#' \donttest{
#' exportVisualStyles('/fullpath/myStyle')
#' exportVisualStyles('/fullpath/myStyle', type = 'JSON')
#' exportVisualStyles('/fullpath/myStyle', style = 'Minimal,default,Directed')
#' }
#' @seealso importVisualStyles
#' @importFrom R.utils isAbsolutePath
#' @export
exportVisualStyles<-function(filename=NULL, type="XML", styles=NULL, base.url=.defaultBaseUrl){
    cmd.string <- 'vizmap export'  # minmum command
    if(is.null(filename))
        filename <- "styles"
    if(!is.null(styles))
        cmd.string <- paste0(cmd.string,' styles="',styles,'"')
    
    ext <- paste0(".",tolower(type),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower(type))
    if(!isAbsolutePath(filename))
        filename <- paste(getwd(),filename,sep="/")
    if (file.exists(filename))
        warning("This file already exists. A Cytoscape popup 
                will be generated to confirm overwrite.",
                call. = FALSE,
                immediate. = TRUE)
    
    cmd.string <- paste0(cmd.string,' OutputFile="',filename,'"',
                         ' options="',type,'"')

    
    commandsPOST(cmd.string, base.url = base.url)
    
}

# ------------------------------------------------------------------------------
#' @title Import Visual Styles
#'
#' @description Loads styles from an XML file and returns the names of the 
#' loaded styles.
#' @param filename (char) Name of the style file to load. Only reads XML files.
#' Default is "styles.xml".
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return (list) Names of styles loaded
#' @examples \donttest{
#' importVisualStyles()
#' }
#' @seealso exportVisualStyles
#' @export
importVisualStyles<-function(filename="styles.xml", base.url=.defaultBaseUrl){
    if(!isAbsolutePath(filename))
        filename = paste(getwd(),filename,sep='/')
    
    cmd.string <- paste0('vizmap load file',' file="',filename,'"')
    commandsPOST(cmd.string, base.url = base.url)
    }

# ------------------------------------------------------------------------------
#' @title Get Visual Style Names
#'
#' @description Retrieve a list of all visual style names.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of names
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
#' @description Apply a visual style to a network.
#' @param style.name Name of a visual style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setVisualStyle()
#' }
#' @export
setVisualStyle <- function(style.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID = getNetworkSuid(network,base.url)
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
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
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
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
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
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
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
#' @description Retrieve the names of all possible visual properties.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of names
#' @examples \donttest{
#' getVisualPropertyNames()
#' }
#' @export
getVisualPropertyNames <- function (base.url=.defaultBaseUrl) {
    res <- cyrestGET("styles/default/defaults", base.url = base.url)
    visual.properties <- unname(res[[1]])
    visual.properties <- vapply(visual.properties, '[[', character(1), 1)
    return(visual.properties)
}



