# ==============================================================================
# Functions affecting the USER INTERFACE, such as panel management. 
# 
# Dev Note: ui/lod is toggleGraphicsDetails() in NetworkViews.R
# ------------------------------------------------------------------------------
#' @title Dock Panel
#'
#' @description Dock a panel back into the UI of Cytoscape.
#' @param panel.name Name of the panel. Multiple ways of referencing panels is supported:\cr
#' WEST, control panel, control, c \cr
#' SOUTH, table panel, table, ta \cr
#' SOUTH_WEST, tool panel, tool, to \cr
#' EAST, results panel, results, r
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' dockPanel('table')
#' }
#' @export
dockPanel <- function(panel.name,base.url=.defaultBaseUrl) {
    panel.name <- .checkPanelName(panel.name)
    panel.name.state <- list(name=panel.name, state='DOCK')
    cyrestPUT("ui/panels", body=list(panel.name.state), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Float Panel
#'
#' @description Pop out a panel from the UI of Cytoscape. Other panels will 
#' expand into the space.
#' @param panel.name Name of the panel. Multiple ways of referencing panels is supported:\cr
#' WEST, control panel, control, c \cr
#' SOUTH, table panel, table, ta \cr
#' SOUTH_WEST, tool panel, tool, to \cr
#' EAST, results panel, results, r
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' floatPanel('table')
#' }
#' @export
floatPanel <- function(panel.name,base.url=.defaultBaseUrl) {
    panel.name <- .checkPanelName(panel.name)
    panel.name.state <- list(name=panel.name, state='FLOAT')
    cyrestPUT("ui/panels", body=list(panel.name.state), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Hide Panel
#'
#' @description Hide a panel in the UI of Cytoscape. Other panels will expand into
#' the space.
#' @param panel.name Name of the panel. Multiple ways of referencing panels is supported:\cr
#' WEST, control panel, control, c \cr
#' SOUTH, table panel, table, ta \cr
#' SOUTH_WEST, tool panel, tool, to \cr
#' EAST, results panel, results, r
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' hidePanel('table')
#' }
#' @export
hidePanel <- function(panel.name,base.url=.defaultBaseUrl) {
    panel.name <- .checkPanelName(panel.name)
    panel.name.state <- list(name=panel.name, state='HIDE')
    cyrestPUT("ui/panels", body=list(panel.name.state), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Hide All Panels
#'
#' @description Hide control, table, tool and results panels.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' hideAllPanels()
#' }
#' @export
hideAllPanels <- function(base.url=.defaultBaseUrl) {
    hidePanel("SOUTH",base.url)
    hidePanel("EAST",base.url)
    hidePanel("WEST",base.url)
    hidePanel("SOUTH_WEST",base.url)
}

# ------------------------------------------------------------------------------
# internal utility function to validate and support references to panels by name
.checkPanelName <- function(panel.name){
    if (tolower(panel.name) %in% c('table panel', 'table', 'ta')){
        panel.name <- 'SOUTH'
    }else if (tolower(panel.name) %in% c('tool panel', 'tool', 'to')){
        panel.name <- 'SOUTH_WEST'
    }else if (tolower(panel.name) %in% c('control panel', 'control', 'c')){
        panel.name <- 'WEST'
    }else if (tolower(panel.name) %in% c('results panel', 'results', 'r')){
        panel.name <- 'EAST'
    }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
        stop (sprintf ('Define a valid panel name.'), stderr ())
    }
    return(panel.name)
}

