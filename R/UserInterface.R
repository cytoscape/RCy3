# ==============================================================================
# Functions affecting the USER INTERFACE, such as panel management. 
# 
# Dev Note: ui/lod is toggleGraphicsDetails() in NetworkViews.R
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

# ------------------------------------------------------------------------------
hideAllPanels <- function(base.url=.defaultBaseUrl) {
    hidePanel("SOUTH")
    hidePanel("EAST")
    hidePanel("WEST")
    hidePanel("SOUTH_WEST")
}



