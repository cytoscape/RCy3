#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1
setGeneric ('getNumberOfCores', function (obj) standardGeneric('getNumberOfCores'))
setGeneric ('getMemoryStatus', function (obj) standardGeneric('getMemoryStatus'))

#http://localhost:1234/v1/gc
setGeneric ('runJavaGarbageCollection', function (obj) standardGeneric('runJavaGarbageCollection'))

# ------------------------------------------------------------------------------

setGeneric ('ping', 	 	  signature='obj', function (obj) standardGeneric('ping'))
setGeneric ('getVersionInfo', function (obj) standardGeneric('getVersionInfo'))
setGeneric ('checkCytoscapeVersion', function (obj) standardGeneric('checkCytoscapeVersion'))
setGeneric ('apiVersion', 	 function (obj=CytoscapeConnection()) standardGeneric('apiVersion'))

#------------------------------------------------------------------------------------------------------------------------
#' Ping Cytoscape
#' 
#' @description Test the connection to Cytoscape via CyREST 
#' @param obj (optional) \code{CytoscapeConnection} 
#' @return status message
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' ping()
#' 
#' cy <- CytoscapeConnection()
#' ping(cy)
#' }
#' @export

#' @rdname ping
setMethod('ping','missing', 
          function() {
              ping(CytoscapeConnection());
          });

#' @rdname ping
setMethod('ping', 'CytoscapeConnectionClass',
          function(obj) {
              conn.str <- paste(obj@uri, obj@api, 'version', sep="/")
              res <- GET(conn.str)
              apiVersion <- fromJSON(rawToChar(res$content))[[1]]
              
              if(length(apiVersion) > 0) {
                  return("You are connected to Cytoscape!")
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }) # END ping


#------------------------------------------------------------------------------------------------------------------------
#' Get Cytoscape and CyREST API Versions
#' 
#' @description Get the running versions of Cytoscape and the CyREST API
#' @param obj (optional) \code{CytoscapeConnection} 
#' @return \code{list} of versions
#' @author Alexander Pico
#' @examples \donttest{
#' getVersionInfo()
#' 
#' cy <- CytoscapeConnection()
#' getVersionInfo(cy)
#' }
#' @export

#' @rdname getVersionInfo
setMethod('getVersionInfo','missing', 
          function() {
              getVersionInfo(CytoscapeConnection());
          });

#' @rdname getVersionInfo
setMethod('getVersionInfo', 'CytoscapeConnectionClass',
          function(obj) {
              conn.str <- paste(obj@uri, obj@api, 'version', sep="/")
              res <- GET(conn.str)
              versions <- fromJSON(rawToChar(res$content))
              if(length(versions[[1]]) == 0) {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
              return(versions)
          }) # END getVersionInfo

#------------------------------------------------------------------------------------------------------------------------
setMethod('apiVersion', 'OptionalCyObjClass', 
          function(obj) {
              res <- GET(obj@uri)
              # get vector with available plugin versions
              available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
              
              api.version <- character(0)
              
              # loop through the vector and check which is the correct plugin version
              for(i in 1:length(available.api.versions)) {
                  server.status = getServerStatus(obj@uri, available.api.versions[i])
                  
                  if(server.status$status_code == 200) {
                      api.version = fromJSON(rawToChar(server.status$content))$apiVersion
                  }
              }
              # current api.version will be the highest/latest version
              return(api.version)
          }) # END apiVersion

# ------------------------------------------------------------------------------
#' Check the version of Cytoscape
#'
#' @param base.url cyrest base url for communicating with cytoscape
#' @return Cytoscape version
#' @export
#' @import httr
#' @import RJSONIO

setMethod('checkCytoscapeVersion', 'OptionalCyObjClass', 
          function(obj) {
    checkversion.url = paste(obj@uri, obj@api, "version", sep="/")
    res = GET(checkversion.url)
    cytoscape.version = fromJSON(rawToChar(res$content))[2][[1]]
    return(cytoscape.version)
})

