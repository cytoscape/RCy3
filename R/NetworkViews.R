# ------------------------------------------------------------------------------
#' @export
getNodeSize <- function (node.names=NULL, network=NULL, base.url =.defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    net.views.SUIDs <- cyrestGET(paste("networks", net.SUID, "views", sep="/"),base.url=base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    widths <- c()
    heights <- c()
    if(is.null(node.names))
        node.names <- getAllNodes()
    node.SUIDs <- .nodeNameToNodeSUID(node.names, net.SUID, base.url)
    for (node.SUID in node.SUIDs){
        # request 
        res <- cyrestGET(
            paste("networks", net.SUID, "views", view.SUID, "nodes", as.character(node.SUID), sep="/"),
            base.url = base.url)
        visual.properties <- sapply(res, '[[', "visualProperty")
        visual.values <- sapply(res, '[[', "value")
        widths <- c(widths, as.integer(visual.values[which(visual.properties =="NODE_WIDTH")]))
        heights <- c(heights, as.integer(visual.values[which(visual.properties =="NODE_HEIGHT")]))         
    } 
    return (list (nodes=node.names, width=widths, height=heights))
}

# ------------------------------------------------------------------------------
#' @export
fitContent <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("apply/fit", net.SUID, sep="/"),base.url = base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @export
fitSelectedContent <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    cur.SUID <- getNetworkSuid('current')
    commandsPOST(paste0('view set current network=SUID:"',net.SUID,'"'), base.url = base.url)
    commandsPOST('view fit selected', base.url = base.url)
    commandsPOST(paste0('view set current network=SUID:"',cur.SUID,'"'), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getCenter <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    
    # get all Cytoscape views belonging to that network
    net.views.SUIDs <- getNetworkViews(net.SUID,base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
        write(sprintf("RCy3::getCenter() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    
    x.coordinate <- cyrestGET(
        paste("networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/"),
        base.url = base.url)$value[[1]]
    y.coordinate <- cyrestGET(
        paste("networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/"),
        base.url = base.url)$value[[1]]
    return(list(x = x.coordinate, y = y.coordinate))
}

# ------------------------------------------------------------------------------
# this method could be used to pan and scroll the Cytoscape canvas, which is adjusted(moved) 
# so that the specified x and y coordinates are at the center of the visible window.
#' @export
setCenter <- function(x, y, network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
        write(sprintf("RCy3::setCenter() - %d views found... setting coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    
    cyrestPUT( 
        paste("networks", net.SUID, "views", view.SUID, "network", sep="/"),
        body = list(list(visualProperty="NETWORK_CENTER_X_LOCATION", value=x),
             list(visualProperty="NETWORK_CENTER_Y_LOCATION", value=y)),
        base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
getZoom <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
        write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    
    res <- cyrestGET(
        paste("networks", net.SUID, "views", view.SUID, "network/NETWORK_SCALE_FACTOR", sep="/"),
        base.url = base.url)
    return(res$value[[1]])
}

# ------------------------------------------------------------------------------
#' @export
setZoom <- function(new.level, network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    net.views.SUIDs <- getNetworkViews(net.SUID, base.url)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
        write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first view", length(net.views.SUIDs)), stderr())
    }
    
    cyrestPUT( 
        paste("networks", net.SUID, "views", view.SUID, "network", sep="/"),
        body = list(list(visualProperty='NETWORK_SCALE_FACTOR', value=new.level)),
        base.url = base.url)
}

#' Exports the current network view as an image
#'
#' @details The image is cropped per the current view in Cytoscape.
#' @param filename (\code{character}) Name of the image file to save. By default, the view's title 
#' is used as the file name and the last valid export path from the current session is used.
#' @param type (\code{character}) Type of image to export, e.g., JPEG, PDF, PNG, PostScript, SVG (case sensitive).
#' @param resolution (\code{numeric}) The resolution of the exported image, in DPI. Valid 
#' only for bitmap formats, when the selected width and height 'units' is inches. The 
#' possible values are: 72 (default), 100, 150, 300, 600. 
#' @param units (\code{character}) The units for the 'width' and 'height' values. Valid 
#' only for bitmap formats, such as PNG and JPEG. The possible values are: pixels (default), inches.
#' @param height (\code{numeric}) The height of the exported image. Valid only for bitmap 
#' formats, such as PNG and JPEG. 
#' @param width (\code{numeric}) The width of the exported image. Valid only for bitmap 
#' formats, such as PNG and JPEG. 
#' @param zoom (\code{numeric}) The zoom value to proportionally scale the image. The default 
#' value is 100.0. Valid only for bitmap formats, such as PNG and JPEG
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' exportImage('myNetwork','PDF')
#' }
#' @export

exportImage<-function(filename=NULL, type=NULL, resolution=NULL, units=NULL, height=NULL, 
                      width=NULL, zoom=NULL, base.url=.defaultBaseUrl){
    cmd.string <- 'view export' # minimum required command
    if(!is.null(filename))
        cmd.string <- paste0(cmd.string,' OutputFile="',filename,'"')
    if(!is.null(type))
        cmd.string <- paste0(cmd.string,' options="',type,'"')
    if(!is.null(resolution))
        cmd.string <- paste0(cmd.string,' Resolution="',resolution,'"')
    if(!is.null(units))
        cmd.string <- paste0(cmd.string,' Units="',units,'"')
    if(!is.null(height))
        cmd.string <- paste0(cmd.string,' Height="',height,'"')
    if(!is.null(width))
        cmd.string <- paste0(cmd.string,' Width="',width,'"')
    if(!is.null(zoom))
        cmd.string <- paste0(cmd.string,' Zoom="',zoom,'"')
    
    commandsPOST(cmd.string)
}

# ------------------------------------------------------------------------------
#' @export
getNetworkViews <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("networks", net.SUID, "views", sep="/"),base.url = base.url)
    return(res)
}
