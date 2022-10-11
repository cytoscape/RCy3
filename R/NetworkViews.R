# ==============================================================================
# Functions for performing VIEW operations in addition to getting and setting 
# view properties. 
# 
# Dev Notes: refer to StyleValues.R, StyleDefaults.R and StyleBypasses.R for 
# getting/setting node, edge and network visual properties via VIEW operations.
# ------------------------------------------------------------------------------
#' @title Create Network View
#'
#' @description Create a network view if one does not already exist
#' @param layout (optional) If TRUE (default), the preferred layout will be 
#' applied to the new view. If FALSE, no layout will be applied.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @details For networks larger than the view creation threashold, a network 
#' view is not created by default.  This function creates a network view if 
#' one does not already exist.
#' @return Network view SUID
#' @examples \donttest{
#' getNetworkViews()
#' }
#' @export
createView <- function(layout = TRUE, network=NULL, base.url =.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network,base.url)
    netview.SUID <- getNetworkViewSuid(network,base.url)
    
    if (!is.null(netview.SUID))
        return (netview.SUID)
    
    res <- commandsPOST(paste0('view create network=SUID:',net.SUID,
                        ' layout=', tolower(layout)), 
                 base.url = base.url)
    return(unname(res[[1]]["view"]))
    
}
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
    net.SUID <- getNetworkSuid(network,base.url)
    
    res <- NULL
    tryCatch(
        expr = {
            res <- cyrestGET(paste("networks", net.SUID, "views", sep="/"),
                         base.url = base.url)
            },
        error=function(e){}
    )
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
#' @return (\code{numeric}) Network view suid. The first (presumably only) view 
#' associated a network is returned.
#' @author Alexander Pico
#' @examples
#' \donttest{
#' getNetworkViewSuid()
#' getNetworkViewSuid("myNetwork")
#' # 90
#' }
#' @export
getNetworkViewSuid <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)

    any.views <- getNetworkViews(net.SUID,base.url)
    
    if(is.null(any.views)){
        return(NULL)
    }
    else if(length(any.views)>1) {
        message("Warning: This network has multiple views. Returning last.")
        tail(any.views,n=1)
    } else {
        any.views
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
    view.SUID <- getNetworkViewSuid(network,base.url)
    if(selected.only){
        commandsPOST(paste0('view fit selected view=SUID:',view.SUID), base.url = base.url)
    } else {
        commandsPOST(paste0('view fit content view=SUID:',view.SUID), base.url = base.url)
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
    view.SUID <- getNetworkViewSuid(network,base.url)
        commandsPOST(paste0('view set current view=SUID:"',view.SUID,'"'), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export Image
#' 
#' @description Saves the current network view as an image file.
#' @details The image is cropped per the current view in Cytoscape. Consider
#' applying \code{\link{fitContent}} prior to export.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param type (\code{character}) Type of image to export, e.g., PNG (default), 
#' JPEG, PDF, SVG, PS (PostScript). 
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
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportImage('/fullpath/myNetwork','PDF')
#' }
#' @export
exportImage<-function(filename=NULL, type="PNG", resolution=NULL, units=NULL, height=NULL, 
                      width=NULL, zoom=NULL, network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    cmd.string <- 'view export' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
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
    ext <- paste0(".",tolower(type),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower(type))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' OutputFile="',getAbsSandboxPath(filename),'"',
                        ' options="',toupper(type),'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export PNG
#' 
#' @description Saves the current network view as an png file.
#' @details Exports the current view to a graphics file and returns the path to 
#' the saved file. To speed up image export for large networks use the option 'allGraphicsDetails=FALSE'.
#' Available for Cytoscape v3.10 or later.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param zoom (\code{numeric}) The zoom value to proportionally scale the image. The default 
#' value is 100.0. Valid only for bitmap formats, such as PNG and JPEG
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param allGraphicsDetails (optional): TRUE results in image with highest detail; False allows faster image
#' generation. The default is TRUE.
#' @param hideLabels (optional): TRUE makes node and edge labels invisible in image. False allows them to be
#' drawn. The default is False.
#' @param transparentBackground (optional): TRUE causes background to be transparent. The default is FALSE.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportPNG('/fullpath/myNetwork')
#' }
#' @export
exportPNG<-function(filename=NULL, allGraphicsDetails=TRUE, hideLabels=FALSE, transparentBackground=FALSE,
                    zoom=NULL, network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    .verifySupportedVersions(1, "3.10")
    cmd.string <- 'view export png' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
    if(!is.null(allGraphicsDetails))
        cmd.string <- paste0(cmd.string,' allGraphicsDetails="',allGraphicsDetails,'"')
    if(!is.null(hideLabels))
        cmd.string <- paste0(cmd.string,' hideLabels="',hideLabels,'"')
    if(!is.null(transparentBackground))
        cmd.string <- paste0(cmd.string,' transparentBackground="',transparentBackground,'"')
    if(!is.null(zoom))
        cmd.string <- paste0(cmd.string,' zoom="',zoom,'"')
    ext <- paste0(".",tolower("PNG"),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower("PNG"))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' outputFile="',getAbsSandboxPath(filename),'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export JPG
#'
#' @description Saves the current network view as an jpg file.
#' @details Exports the current view to a graphics file and returns the path to 
#' the saved file. To speed up image export for large networks use the option 'allGraphicsDetails=FALSE'.
#' Available for Cytoscape v3.10 or later.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param zoom (\code{numeric}) The zoom value to proportionally scale the image. The default 
#' value is 100.0. Valid only for bitmap formats, such as PNG and JPEG
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param allGraphicsDetails (optional): TRUE results in image with highest detail; False allows faster image
#' generation. The default is TRUE.
#' @param hideLabels (optional): TRUE makes node and edge labels invisible in image. False allows them to be
#' drawn. The default is False.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportJPG('/fullpath/myNetwork')
#' }
#' @export
exportJPG<-function(filename=NULL, allGraphicsDetails=TRUE, hideLabels=FALSE,
                    zoom=NULL, network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    .verifySupportedVersions(1, "3.10")
    cmd.string <- 'view export jpg' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
    if(!is.null(allGraphicsDetails))
        cmd.string <- paste0(cmd.string,' allGraphicsDetails="',allGraphicsDetails,'"')
    if(!is.null(hideLabels))
        cmd.string <- paste0(cmd.string,' hideLabels="',hideLabels,'"')
    if(!is.null(zoom))
        cmd.string <- paste0(cmd.string,' zoom="',zoom,'"')
    ext <- paste0(".",tolower("JPG"),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower("JPG"))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' outputFile="',getAbsSandboxPath(filename),'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export PDF
#'
#' @description Saves the current network view as an pdf file.
#' @details Exports the current view to a graphics file and returns the path to 
#' the saved file. To speed up image export for large networks use the option 'allGraphicsDetails=FALSE'.
#' Available for Cytoscape v3.10 or later.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param hideLabels (optional): TRUE makes node and edge labels invisible in image. False allows them to be
#' drawn. The default is False.
#' @param exportTextAsFont (optional): If TRUE (the default value), texts will be exported as fonts.
#' @param orientation (optional): Page orientation, portrait or landscape.
#' @param pageSize (optional): (Auto|Letter|Legal|Tabloid|A0|A1|A2|A3|A4|A5)
#' Predefined standard page size, or choose custom. Default is 'Letter'.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportPDF('/fullpath/myNetwork')
#' }
#' @export
exportPDF<-function(filename=NULL, exportTextAsFont=TRUE, hideLabels=FALSE, pageSize="Letter",
                    orientation="Portrait", network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    .verifySupportedVersions(1, "3.10")
    cmd.string <- 'view export pdf' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
    if(!is.null(exportTextAsFont))
        cmd.string <- paste0(cmd.string,' exportTextAsFont="',exportTextAsFont,'"')
    if(!is.null(hideLabels))
        cmd.string <- paste0(cmd.string,' hideLabels="',hideLabels,'"')
    if(!is.null(pageSize))
        cmd.string <- paste0(cmd.string,' pageSize="',pageSize,'"')
    if(!is.null(orientation))
        cmd.string <- paste0(cmd.string,' orientation="',orientation,'"')
    ext <- paste0(".",tolower("PDF"),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower("PDF"))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' outputFile="',getAbsSandboxPath(filename),'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export PS
#'
#' @description Saves the current network view as an ps file.
#' @details Exports the current view to a graphics file and returns the path to 
#' the saved file. To speed up image export for large networks use the option 'allGraphicsDetails=FALSE'.
#' Available for Cytoscape v3.10 or later.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param hideLabels (optional): TRUE makes node and edge labels invisible in image. False allows them to be
#' drawn. The default is False.
#' @param exportTextAsFont (optional): If TRUE (the default value), texts will be exported as fonts.
#' @param orientation (optional): Page orientation, portrait or landscape.
#' @param pageSize (optional): (Auto|Letter|Legal|Tabloid|A0|A1|A2|A3|A4|A5)
#' Predefined standard page size, or choose custom. Default is 'Letter'.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportPS('/fullpath/myNetwork')
#' }
#' @export
exportPS<-function(filename=NULL, exportTextAsFont=TRUE, hideLabels=FALSE,
                    network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    .verifySupportedVersions(1, "3.10")
    cmd.string <- 'view export ps' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
    if(!is.null(exportTextAsFont))
        cmd.string <- paste0(cmd.string,' exportTextAsFont="',exportTextAsFont,'"')
    if(!is.null(hideLabels))
        cmd.string <- paste0(cmd.string,' hideLabels="',hideLabels,'"')
    ext <- paste0(".",tolower("PS"),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower("PS"))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' outputFile="',getAbsSandboxPath(filename),'"',
                        ' view=SUID:"',view.SUID,'"'), 
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' Export SVG
#'
#' @description Saves the current network view as an svg file.
#' @details Exports the current view to a graphics file and returns the path to 
#' the saved file. To speed up image export for large networks use the option 'allGraphicsDetails=FALSE'.
#' Available for Cytoscape v3.10 or later.
#' @param filename (\code{character}) Full path or path relavtive to current 
#' working directory, in addition to the name of the file. Extension is 
#' automatically added based on the \code{type} argument. If blank, the current
#' network name will be used.
#' @param network (optional) Name or SUID of a network or view. Default is the 
#' "current" network active in Cytoscape. The first (presummably only) view 
#' associated a network is used if a specific view SUID is not provided.
#' @param hideLabels (optional): TRUE makes node and edge labels invisible in image. False allows them to be
#' drawn. The default is False.
#' @param exportTextAsFont (optional): If TRUE (the default value), texts will be exported as fonts.
#' @param orientation (optional): Page orientation, portrait or landscape.
#' @param pageSize (optional): (Auto|Letter|Legal|Tabloid|A0|A1|A2|A3|A4|A5)
#' Predefined standard page size, or choose custom. Default is 'Letter'.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows Cytoscape show a message box before overwriting the file if the file already
#' exists; TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @examples
#' \donttest{
#' exportSVG('/fullpath/myNetwork')
#' }
#' @export
exportSVG<-function(filename=NULL, exportTextAsFont=TRUE, hideLabels=FALSE,
                   network=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    .verifySupportedVersions(1, "3.10")
    cmd.string <- 'view export svg' # a good start
    
    # filename must be supplied
    if(is.null(filename))
        filename <- getNetworkName(network,base.url)
    
    # view must be supplied
    view.SUID <- getNetworkViewSuid(network,base.url)
    
    # optional args
    if(!is.null(exportTextAsFont))
        cmd.string <- paste0(cmd.string,' exportTextAsFont="',exportTextAsFont,'"')
    if(!is.null(hideLabels))
        cmd.string <- paste0(cmd.string,' hideLabels="',hideLabels,'"')
    ext <- paste0(".",tolower("SVG"),"$")
    if (!grepl(ext,filename))
        filename <- paste0(filename,".",tolower("SVG"))
    fileInfo <- sandboxGetFileInfo(filename, base.url=base.url)
    if (length(fileInfo[['modifiedTime']] == 1) && fileInfo[['isFile']]){
        if (overwriteFile){
            sandboxRemoveFile(filename, base.url=base.url)
        }
        else {
            warning("This file already exists. A Cytoscape popup will be 
                    generated to confirm overwrite.",
                    call. = FALSE,
                    immediate. = TRUE)
        }
    }
    commandsPOST(paste0(cmd.string,
                        ' outputFile="',getAbsSandboxPath(filename),'"',
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
