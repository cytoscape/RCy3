# ==============================================================================
# Functions for getting and setting style DEPEDENDENCIES,
# organized into sections:
# 
# I. General functions for getting and setting dependencies
# II. Specific functions for setting particular dependencies
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' @title Get the values of dependencies in a style
#'
#' @description Updates style dependencies, overriding any prior settings.
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' getStyleDependencies("myStyle")
#' }
#' @importFrom stats setNames
#' @export
#' @section Available Dependencies:
#' arrowColorMatchesEdge
#' nodeCustomGraphicsSizeSync
#' nodeSizeLocked

getStyleDependencies <- function(style.name="default",base.url=.defaultBaseUrl){
    
    # launch error if visual style name is missing
    if (! style.name %in% getVisualStyleNames (base.url)) {
        write (sprintf ('Error in RCy3::lockNodeDimensions. No visual style named "%s"', style.name), stdout ())
        return ()
    }
    res <- cyrestGET(paste('styles', style.name, 'dependencies', sep = '/'),
              base.url = base.url)
    
    # make it a named list
    dep.en <- lapply(res, function(x) x$enabled)
    dep.vi <- lapply(res, function(x) x$visualPropertyDependency)
    deps <- setNames(unlist(dep.en),unlist(dep.vi))
    return(deps)
}

# ------------------------------------------------------------------------------
#' @title Set Style Dependencies
#'
#' @description Sets the values of dependencies in a style, overriding any prior settings.
#' @param style.name Name of style; default is "default" style
#' @param dependencies A \code{list} of style dependencies, see Available Dependencies
#' below. Note: each dependency is set by a boolean, TRUE or FALSE (T or F)
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' setStyleDependencies("myStyle",list(nodeSizeLocked=TRUE))
#' }
#' @export
#' @section Available Dependencies:
#' arrowColorMatchesEdge
#' nodeCustomGraphicsSizeSync
#' nodeSizeLocked

setStyleDependencies <- function(style.name="default",dependencies,base.url=.defaultBaseUrl){
    
    # launch error if visual style name is missing
    if (! style.name %in% getVisualStyleNames (base.url)) {
        write (sprintf ('Error in RCy3::lockNodeDimensions. No visual style named "%s"', style.name), stdout ())
        return ()
    }
    dep.list <- list()
    for (i in 1:length(dependencies)) {
        dep.list[[i]] <- list(visualPropertyDependency=names(dependencies)[i],
                              enabled=dependencies[[i]])
    }
    cyrestPUT(paste('styles', style.name, 'dependencies', sep = '/'),
              body=dep.list,base.url = base.url)
    invisible(commandsPOST(paste0('vizmap apply styles="',style.name,'"')))
}

# ==============================================================================
# I. Specific Functions
# Pattern: (1) Call setStyleDependencies()
# ------------------------------------------------------------------------------
#' @title Match Arrow Color To Edge
#'
#' @description Set a boolean value to have arrow shapes share the same color as 
#' the edge.
#' @param new.state (Boolean) Whether to match arrow color to edge.
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' matchArrowColorToEdge(TRUE)
#' }
#' @export
matchArrowColorToEdge <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    toggle='false'
    if(new.state)
        toggle='true'
    setStyleDependencies(style.name = style.name, 
                            dependencies = list(arrowColorMatchesEdge=toggle),
                            base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Lock Node Dimensions
#'
#' @description Set a boolean value to have node width and height fixed to a 
#' single size value.
#' @param new.state (Boolean) Whether to lock node width and height
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' lockNodeDimensions(TRUE)
#' }
#' @export
lockNodeDimensions <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    toggle='false'
    if(new.state)
        toggle='true'
    setStyleDependencies(style.name = style.name, 
                            dependencies = list(nodeSizeLocked=toggle),
                            base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Sync Node Custom Graphics Size
#'
#' @description Set a boolean value to have the size of custom graphics match that
#' of the node.
#' @param new.state (Boolean) Whether to sync node custom graphics size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' syncNodeCustomGraphicsSize(TRUE)
#' }
#' @export
syncNodeCustomGraphicsSize <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    toggle='false'
    if(new.state)
        toggle='true'
    setStyleDependencies(style.name = style.name, 
                            dependencies = list(nodeCustomGraphicsSizeSync=toggle),
                            base.url = base.url)
}
