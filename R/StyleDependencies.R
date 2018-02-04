# ------------------------------------------------------------------------------
#' Updates the values of dependencies in a style
#'
#' @description Updates style dependencies, overriding any prior settings.
#' @param style.name (char) name for style
#' @param dependencies A \code{list} of style dependencies, see Available Dependencies
#' below. Note: each dependency is set by a boolean, TRUE or FALSE (T or F)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDependencies("myStyle",list(nodeSizeLocked=TRUE))
#' }
#' @export
#' @section Available Dependencies:
#' arrowColorMatchesEdge
#' nodeCustomGraphicsSizeSync
#' nodeSizeLocked

updateStyleDependencies <- function(style.name,dependencies,base.url=.defaultBaseUrl){
    
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

#------------------------------------------------------------------------------------------------------------------------
#' @export
matchArrowColorToEdge <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    updateStyleDependencies(style.name = style.name, 
                            dependencies = list(arrowColorMatchesEdge=new.state),
                            base.url = base.url)
}

#' @export
lockNodeDimensions <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    updateStyleDependencies(style.name = style.name, 
                            dependencies = list(nodeSizeLocked=new.state),
                            base.url = base.url)
}

#' @export
syncNodeCustomGraphicsSize <- function (new.state, style.name='default', base.url=.defaultBaseUrl) {
    updateStyleDependencies(style.name = style.name, 
                            dependencies = list(nodeCustomGraphicsSizeSync=new.state),
                            base.url = base.url)
}
