#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

# ------ TODO -----------------------------------------------------------------

# ------------------------------------------------------------------------------
setGeneric ('showGraphicsDetails',		 signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.value) standardGeneric ('showGraphicsDetails'))
setGeneric ('hidePanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('hidePanel'))
setGeneric ('hideAllPanels',			 signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('hideAllPanels'))
setGeneric ('dockPanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('floatPanel'))

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


setMethod ('showGraphicsDetails', 'OptionalCyObjClass',
           
           function (obj, new.value) {
               resource.uri <- paste(obj@uri, obj@api, "ui/lod/", sep="/")
               request.res <- PUT(resource.uri)
               invisible (request.res)
               if (class (obj) == 'CytoscapeWindowClass'){
                   redraw (obj)
               }
               write(sprintf('RCy3::showGraphicsDetails(), Switching between show and hide full graphics details.'), stdout())
               
           })

# ------------------------------------------------------------------------------
setMethod('hidePanel', 'OptionalCyObjClass', 
          function(obj, panel.name) {
              
              
              if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
                  panel.name <- 'SOUTH'
              }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
                  panel.name <- 'WEST'
              }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
                  write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
                  return(NA)
              }
              
              panel.name.state = list(name=panel.name, state='HIDE')
              
              resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
              request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
              
              invisible(request.res)
          })
## END hidePanel

# ------------------------------------------------------------------------------
setMethod('hideAllPanels', 'OptionalCyObjClass', 
          function(obj) {
              hidePanel(obj, "SOUTH")
              hidePanel(obj, "EAST")
              hidePanel(obj, "WEST")
              hidePanel(obj, "SOUTH_WEST")
          })
## END hideAllPanels

# ------------------------------------------------------------------------------
setMethod('dockPanel', 'OptionalCyObjClass', 
          function(obj, panel.name) {
              
              
              if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
                  panel.name <- 'SOUTH'
              }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
                  panel.name <- 'WEST'
              }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
                  write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
                  return(NA)
              }
              
              panel.name.state = list(name=panel.name, state='DOCK')
              
              resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
              request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
              
              invisible(request.res)
          })
## END dockPanel

# ------------------------------------------------------------------------------
setMethod('floatPanel', 'OptionalCyObjClass', 
          function(obj, panel.name) {
              
              
              if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
                  panel.name <- 'SOUTH'
              }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
                  panel.name <- 'WEST'
              }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
                  write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
                  return(NA)
              }
              
              panel.name.state = list(name=panel.name, state='FLOAT')
              
              resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
              request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
              
              invisible(request.res)
          })
## END floatPanel
