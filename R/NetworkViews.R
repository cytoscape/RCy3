#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

# ------------------------------------------------------------------------------
setGeneric ('getNodeSize',				 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('getNodeSize'))
setGeneric ('fitContent',				 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('fitContent'))
setGeneric ('fitSelectedContent',		 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('fitSelectedContent'))
setGeneric ('getCenter',				 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getCenter'))
setGeneric ('setCenter',				 signature='obj', function (obj=CytoscapeWindowFromNetwork(), x, y) standardGeneric ('setCenter'))
setGeneric ('getZoom',					 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getZoom'))
setGeneric ('setZoom',					 signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.level) standardGeneric ('setZoom'))
setGeneric ('getViewCoordinates',		 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getViewCoordinates'))
setGeneric ('saveImage',                 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, image.type, h=600) standardGeneric ('saveImage'))

setGeneric ('.getNetworkViews',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('.getNetworkViews'))

# ------------------------------------------------------------------------------
setMethod ('getNodeSize', 'OptionalCyWinClass',
           
           function (obj, node.names) {
               # get network ID 
               net.SUID = as.character(obj@suid)
               
               # get the views for the given network model
               resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", sep="/")
               request.res <- GET(resource.uri)
               net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
               view.SUID <- as.character(net.views.SUIDs[[1]])
               
               widths <- c()
               heights <- c()
               
               node.SUIDs <- .nodeNameToNodeSUID(obj,node.names)
               
               for (node.SUID in node.SUIDs){
                   # request 
                   resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", as.character(node.SUID), sep="/")
                   
                   # request result
                   request.res <- GET(resource.uri)
                   request.res <- fromJSON(rawToChar(request.res$content))
                   visual.properties <- sapply(request.res, '[[', "visualProperty")
                   visual.values <- sapply(request.res, '[[', "value")
                   widths <- c(widths, as.integer(visual.values[which(visual.properties =="NODE_WIDTH")]))
                   heights <- c(heights, as.integer(visual.values[which(visual.properties =="NODE_HEIGHT")]))         
               } # end for (node.name in node.names)
               
               invisible(request.res)
               return (list (width=widths, height=heights))
           }) # getNodeSize

# ------------------------------------------------------------------------------
# display the graph using all of the available window space (the Cytoscape drawing canvas)
setMethod('fitContent', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              resource.uri <- paste(obj@uri, obj@api, "apply/fit", net.SUID, sep="/")
              request.res <- GET(url=resource.uri)
              invisible(request.res)
          })
## END fitContent

# ------------------------------------------------------------------------------
setMethod('fitSelectedContent', 'OptionalCyWinClass', 
          function(obj) {
              write(sprintf("WARNING: Method RCy3::fitSelectedContent() is not implemented in RCy3!"), stderr())
              
              return(FALSE)
          })
## END fitSelectedContent

# ------------------------------------------------------------------------------
setMethod('getCenter', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              
              # get all Cytoscape views belonging to that network
              net.views.SUIDs <- .getNetworkViews(obj)
              view.SUID <- as.character(net.views.SUIDs[[1]])
              
              # if multiple views are found, inform the user about it
              if(length(net.views.SUIDs) > 1) {
                  write(sprintf("RCy3::getCenter() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
              }
              # get the X-coordinate
              resource.uri <- 
                  paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/")
              request.res <- GET(resource.uri)
              x.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
              # get the Y-coordinate
              resource.uri <- 
                  paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/")
              request.res <- GET(resource.uri)
              y.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
              
              return(list(x = x.coordinate, y = y.coordinate))
          })
## END getCenter

# ------------------------------------------------------------------------------
# this method could be used to pan and scroll the Cytoscape canvas, which is adjusted(moved) 
# so that the specified x and y coordinates are at the center of the visible window.
setMethod('setCenter', 'OptionalCyWinClass', 
          function(obj, x, y) {
              net.SUID <- as.character(obj@suid)
              
              net.views.SUIDs <- .getNetworkViews(obj)
              view.SUID <- as.character(net.views.SUIDs[[1]])
              
              # if multiple views are found, inform the user about it
              if(length(net.views.SUIDs) > 1) {
                  write(sprintf("RCy3::setCenter() - %d views found... setting coordinates of the first one", length(net.views.SUIDs)), stderr())
              }
              
              # set the X-coordinate
              resource.uri <- 
                  paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
              new.x.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_X_LOCATION", value=x)))
              request.res <- PUT(resource.uri, body=new.x.coordinate.JSON, encode="json")
              # set the Y-coordinate
              resource.uri <- 
                  paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
              new.y.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_Y_LOCATION", value=y)))
              request.res <- PUT(resource.uri, body=new.y.coordinate.JSON, encode="json")
              invisible(request.res)
          })
## END setCenter

# ------------------------------------------------------------------------------
setMethod('getZoom', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              
              # get the existing views for the given network model
              net.views.SUIDs <- .getNetworkViews(obj)
              view.SUID <- as.character(net.views.SUIDs[[1]])
              
              # if multiple views are found, inform the user about it
              if(length(net.views.SUIDs) > 1) {
                  write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
              }
              
              resource.uri <- 
                  paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_SCALE_FACTOR", sep="/")
              request.res <- GET(resource.uri)
              zoom.level <- fromJSON(rawToChar(request.res$content))$value[[1]]
              
              return(zoom.level)
          })
## END getZoom

# ------------------------------------------------------------------------------
setMethod('setZoom', 'OptionalCyWinClass', 
          function(obj, new.level) {
              net.SUID <- as.character(obj@suid)
              
              net.views.SUIDs <- .getNetworkViews(obj)
              view.SUID <- as.character(net.views.SUIDs[[1]])
              
              # if multiple views are found, inform the user about it
              if(length(net.views.SUIDs) > 1) {
                  write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first view", length(net.views.SUIDs)), stderr())
              }
              
              view.zoom.value <- list(visualProperty='NETWORK_SCALE_FACTOR', value=new.level)
              view.zoom.value.JSON <- toJSON(list(view.zoom.value))
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
              request.res <- PUT(url=resource.uri, body=view.zoom.value.JSON, encode="json")
              
              invisible(request.res)
          })
## END setZoom

# ------------------------------------------------------------------------------
setMethod('getViewCoordinates', 'OptionalCyWinClass', 
          function(obj) {
              write(sprintf("WARNING: Method RCy3::getViewCoordinates() is not implemented in RCy3!"), stderr())
              
              return(FALSE)
          })
## END getViewCoordinates

#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'OptionalCyWinClass',
           
           function (obj, filename, image.type, h = 600) {
               image.type = tolower (image.type)
               stopifnot (image.type %in% c ('png', 'pdf', 'svg'))
               id = as.character (obj@suid)
               
               if (!file.exists(filename)){
                   if(image.type=='png'){
                       
                       resource.uri <- paste(obj@uri, obj@api, "networks", id,
                                             paste0("views/first.", image.type, "?h=", h), sep="/")  
                   } 
                   else{
                       # get the view image from Cytoscape in PNG, PDF, or SVG format
                       resource.uri <- paste(obj@uri, obj@api, "networks", id,
                                             paste0("views/first.", image.type), sep="/")
                   }
                   request.res <- GET(resource.uri, write_disk(paste0(filename,".", image.type), overwrite = TRUE))
                   write (sprintf ('saving image to %s.%s', filename, image.type), stderr ())
               }else{
                   write (sprintf ('choose another filename. File exists: %s', filename), stderr ())
               }
           }) # saveImage
#------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# helper function: returns the SUIDs of all views belonging to specific network
setMethod('.getNetworkViews', 'OptionalCyObjClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", sep="/")
              request.res <- GET(url=resource.uri)
              network.view.SUIDs <- unname(fromJSON(rawToChar(request.res$content)))
              return(network.view.SUIDs)
          }) 
## END .getNetworkViews
