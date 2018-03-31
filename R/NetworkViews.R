# ==============================================================================
# Functions for performing VIEW operations in addition to getting and setting 
# view properties. 
# 
# Dev Notes: refer to StyleValues.R, StyleDefaults.R and StyleBypasses.R for 
# getting/setting node, edge and network visual properties via VIEW operations.
# ------------------------------------------------------------------------------
#' @title Get Network Views
#'
#' @description Retrieve list of network view SUIDs
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of network view SUIDs
#' @examples \donttest{
#' getNetworkViews()
#' }
#' @export
getNetworkViews <- function(network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("networks", net.SUID, "views", sep="/"),base.url = base.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title Get the SUID of a network view
#'
#' @description Retrieve the SUID of a network view
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape. If a network view SUID is provided, 
#' then it is validated and returned.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return (\code{numeric}) Network view suid. The first (presummably only) view 
#' associated a network is returned.
#' @author Alexander Pico
#' @examples
#' \donttest{
#' getNetworkViewSuid()
#' getNetworkViewSuid("myNetwork")
#' # 90
#' }
#' @export
#
# Dev Notes: analogous to getNetworkSuid, this function attempts to handle all 
# of the multiple ways we support network view referencing (e.g., title, SUID, 
# 'current', and NULL). These functions are then used by functions
# that take a "network" argument and requires a view SUID.
# 
getNetworkViewSuid <- function(network = NULL, base.url = .defaultBaseUrl) {

    if (is.character(network)) {
        #network name (or "current") provided, return first view
        getNetworkViews(network)[1]
    } else if (is.numeric(network)) {
        #suid provided, but network or view?
        net.suids <- cyrestGET('networks', base.url = base.url)
        if (network %in% net.suids){ # network SUID, return first view
            getNetworkViews(network)[1]
        } else{
            view.suids <- unlist(lapply(net.suids, function(x) getNetworkViews(x)))
            if (network %in% view.suids){ # view SUID, return it
                return(network)
            } else {
                stop(paste0("Network view does not exist for: ", network))
            }
        }
     } else {
        #use current network, return first view
        network.title = 'current'
        getNetworkViews(network)[1]
    }
}

# ------------------------------------------------------------------------------
#' @title Fit Content
#'
#' @description Zoom and pan network view to maximize either height or width of
#' current network window.
#' @details Takes first (presumably only) view associated with provided network
#' @param selected.only (Boolean) Whether to fit only current selection. Default 
#' is false, i.e., to fit the entire network.
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' fitContent()
#' }
#' @export
fitContent <- function(selected.only=FALSE, network=NULL, 
                       base.url =.defaultBaseUrl) {
    view.SUID <- getNetworkViewSuid(network)
    if(selected.only){
        commandsPOST(paste0('view fit selected view=SUID:"',view.SUID,'"'), base.url = base.url)
    } else {
        commandsPOST(paste0('view fit content view=SUID:"',view.SUID,'"'), base.url = base.url)
    }
}

# ------------------------------------------------------------------------------
#' @title Set Current View
#'
#' @description Set which network view is "current".
#' @details Takes first (presumably only) view associated with provided network
#' @param network (optional) Name or SUID of a network or view. The first 
#' (presummably only) view associated a network is used if a specific view 
#' SUID is not provided.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setCurrentView()
#' }
#' @export
setCurrentView <- function(network = NULL, 
                       base.url =.defaultBaseUrl) {
    view.SUID <- getNetworkViewSuid(network)
        commandsPOST(paste0('view set current view=SUID:"',view.SUID,'"'), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export Image
#' 
#' @description Saves the current network view as an image file.
#' @details The image is cropped per the current view in Cytoscape. Consider
#' applying \code{\link{fitContent}} prior to export.
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
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' exportImage('/fullpath/myNetwork','PDF')
#' }
#' @export
exportImage<-function(filename=NULL, type=NULL, resolution=NULL, units=NULL, height=NULL, 
                      width=NULL, zoom=NULL, network=NULL, base.url=.defaultBaseUrl){
    cmd.string <- 'view export' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network)
    
    # optional args
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
    
    commandsPOST(paste0(cmd.string,
                        ' OutputFile="',filename,'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Toggle Graphics Details
#'
#' @description Regardless of the current zoom level and network size,
#' show (or hide) graphics details, e.g., node labels.
#' @details Displaying graphics details on a very large network will affect pan
#' and zoom performance, depending on your available RAM. 
#' See \link{cytoscapeMemoryStatus}.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' showGraphicsDetails(TRUE)
#' }
#' @export
toggleGraphicsDetails <- function (base.url=.defaultBaseUrl) {
    resource.uri <- paste(base.url, "ui/lod/", sep="/")
    request.res <- PUT(resource.uri)
    invisible (request.res)
}
