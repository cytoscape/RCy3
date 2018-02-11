# ------------------------------------------------------------------------------
#' Open Swagger docs for CyREST API 
#'
#' @description Opens swagger docs in default browser for a live
#' instance of CyREST operations.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return Web page in browser
#' @export
#' @examples
#' \donttest{
#' cyrestAPI()
#' }
#' @importFrom utils browseURL

cyrestAPI<-function(base.url=.defaultBaseUrl){
    browseURL(paste(base.url,'/swaggerUI/swagger-ui/index.html?url=',base.url,'/swagger.json#/',sep=""))
}

# ------------------------------------------------------------------------------
#' Open Swagger docs for CyREST Commands API 
#'
#' @description Opens swagger docs in default browser for a live
#' instance of Commands available via CyREST.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return Web page in browser
#' @export
#' @examples
#' \donttest{
#' commandsAPI()
#' }
#' @importFrom utils browseURL

commandsAPI<-function(base.url=.defaultBaseUrl){
    browseURL(paste(base.url,'/swaggerUI/swagger-ui/index.html?url=',base.url,'/commands/swagger.json#/',sep=""))
}


# ------------------------------------------------------------------------------
#' @title Show Graphics Details
#'
#' @description Regardless of the current zoom level and network size,
#' display (or hide) graphics details, e.g., node labels.
#' @details Displaying graphics details on a very large network will affect pan
#' and zoom performance, depending on your available RAM. 
#' See \link{cytoscapeMemoryStatus}.
#' @param new.value \code{boolean} Whether to show graphic details
#' @param base.url DESCRIPTION
#' @return None
#' @examples \donttest{
#' showGraphicsDetails(TRUE)
#' }
#' @export
showGraphicsDetails <- function (new.value, base.url=.defaultBaseUrl) {
    resource.uri <- paste(base.url, "ui/lod/", sep="/")
    request.res <- PUT(resource.uri)
    invisible (request.res)
}

# ------------------------------------------------------------------------------
hidePanel <- function(panel.name,base.url=.defaultBaseUrl) {
    
    
    if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
        panel.name <- 'SOUTH'
    }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
        panel.name <- 'WEST'
    }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
        write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
        return(NA)
    }
    
    panel.name.state = list(name=panel.name, state='HIDE')
    
    resource.uri <- paste(base.url, "ui/panels", sep="/")
    request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
    
    invisible(request.res)
}
## END hidePanel

# ------------------------------------------------------------------------------
hideAllPanels <- function(base.url=.defaultBaseUrl) {
    hidePanel("SOUTH")
    hidePanel("EAST")
    hidePanel("WEST")
    hidePanel("SOUTH_WEST")
}
## END hideAllPanels

# ------------------------------------------------------------------------------
dockPanel <- function(panel.name,base.url=.defaultBaseUrl) {
    
    
    if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
        panel.name <- 'SOUTH'
    }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
        panel.name <- 'WEST'
    }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
        write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
        return(NA)
    }
    
    panel.name.state = list(name=panel.name, state='DOCK')
    
    resource.uri <- paste(base.url, "ui/panels", sep="/")
    request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
    
    invisible(request.res)
}
## END dockPanel

# ------------------------------------------------------------------------------
floatPanel <- function(panel.name,base.url=.defaultBaseUrl) {
    
    
    if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
        panel.name <- 'SOUTH'
    }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
        panel.name <- 'WEST'
    }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
        write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
        return(NA)
    }
    
    panel.name.state = list(name=panel.name, state='FLOAT')
    
    resource.uri <- paste(base.url, "ui/panels", sep="/")
    request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
    
    invisible(request.res)
}
## END floatPanel
