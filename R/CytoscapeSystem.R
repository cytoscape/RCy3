#' @include CytoscapeConnectionClass.R

# ------------------------------------------------------------------------------
setGeneric ('ping', 	 	            function (base.url) standardGeneric('ping'))
setGeneric ('cytoscapeVersionInfo',           function (base.url) standardGeneric('cytoscapeVersionInfo'))
setGeneric ('availableApiVersions', 	    function (base.url) standardGeneric('availableApiVersions'))
setGeneric ('cytoscapeNumberOfCores',         function (base.url) standardGeneric('cytoscapeNumberOfCores'))
setGeneric ('cytoscapeMemoryStatus',          function (base.url) standardGeneric('cytoscapeMemoryStatus'))
setGeneric ('cytoscapeFreeMemory', function (base.url) standardGeneric('cytoscapeFreeMemory'))

#------------------------------------------------------------------------------------------------------------------------
#' Ping Cytoscape
#' 
#' @description Test the connection to Cytoscape via CyREST 
#' @param base.url (optional) URL prefix for CyREST calls
#' @return status message
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' ping()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' ping(base.url)
#' }
#' @export

#' @rdname ping
setMethod('ping','missing', 
          function() {
              ping(base.url=.defaultBaseUrl);
          })
#' @rdname ping
setMethod('ping', 'character',
          function(base.url) {
              conn.str <- paste(base.url, 'version', sep="/")
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  return("You are connected to Cytoscape!")
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          })
# DEPRECATED
setMethod('ping', 'CytoscapeConnectionClass',
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              ping(base.url = loc.base.url)
          })


#------------------------------------------------------------------------------------------------------------------------
#' Cytoscape and CyREST API Versions
#' 
#' @description Returns the versions of the current Cytoscape and CyREST API
#' @param base.url (optional) URL prefix for CyREST calls
#' @return \code{list} of versions
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeVersionInfo()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' cytoscapeVersionInfo(base.url)
#' }
#' @export

#' @rdname cytoscapeVersionInfo
setMethod('cytoscapeVersionInfo','missing', 
          function() {
              cytoscapeVersionInfo(base.url=.defaultBaseUrl);
          })

#' @rdname cytoscapeVersionInfo
setMethod('cytoscapeVersionInfo', 'character',
          function(base.url) {
              conn.str <- paste(base.url, 'version', sep="/")
              res <- GET(conn.str)
              versions <- fromJSON(rawToChar(res$content))
              if(length(versions[[1]]) == 0) {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
              return(versions)
          }) 
# DEPRECATED
setMethod('cytoscapeVersionInfo', 'CytoscapeConnectionClass',
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              cytoscapeVersionInfo(base.url = loc.base.url)
          }) 

#------------------------------------------------------------------------------------------------------------------------
#' Available CyREST API Versions 
#' 
#' @description Get the list of available CyREST API versions 
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{list} of API versions as \code{character} strings, e.g., "v1"
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' availableApiVersions()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' availableApiVersions(base.url)
#' }
#' @export

#' @rdname availableApiVersions
setMethod('availableApiVersions', 'missing', 
          function() {
              availableApiVersions(base.url = .defaultBaseUrl)
          })
#' @rdname availableApiVersions
setMethod('availableApiVersions', 'character', 
          function(base.url) {
              uri <- strsplit(base.url,'/v')[[1]][1]
              res <- GET(uri)
              available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
              return(available.api.versions)
          }) 
#DEPRECATED
setMethod('availableApiVersions', 'CytoscapeConnectionClass', 
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              availableApiVersions(base.url = loc.base.url)
          })

#------------------------------------------------------------------------------------------------------------------------
#' Number of Processors Available to Cytoscape
#' 
#' @description Returns the processor resources of the server running Cytoscape
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{numeric} value
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeNumberOfCores()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' cytoscapeNumberOfCores(base.url)
#' }
#' @export

#' @rdname cytoscapeNumberOfCores
setMethod('cytoscapeNumberOfCores','missing', 
          function() {
              cytoscapeNumberOfCores(base.url=.defaultBaseUrl);
          })

#' @rdname cytoscapeNumberOfCores
setMethod('cytoscapeNumberOfCores', 'character',
          function(base.url) {
              conn.str <- paste(base.url)
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  status <- fromJSON(rawToChar(res$content))
                  return(status$numberOfCores)
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }) 
# DEPRECATED
setMethod('cytoscapeNumberOfCores', 'CytoscapeConnectionClass',
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              cytoscapeNumberOfCores(base.url = loc.base.url)
          }) 

#------------------------------------------------------------------------------------------------------------------------
#' Memory Available to Cytoscape
#' 
#' @description Returns the memory resources of the server running Cytoscape
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{list} of \code{numeric} values
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeMemoryStatus()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' cytoscapeMemoryStatus(base.url)
#' }
#' @export

#' @rdname cytoscapeMemoryStatus
setMethod('cytoscapeMemoryStatus','missing', 
          function() {
              cytoscapeMemoryStatus(base.url=.defaultBaseUrl);
          })

#' @rdname cytoscapeMemoryStatus
setMethod('cytoscapeMemoryStatus', 'character',
          function(base.url) {
              conn.str <- paste(base.url)
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  status <- fromJSON(rawToChar(res$content))
                  return(status$memoryStatus)
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }) 
# DEPRECATED
setMethod('cytoscapeMemoryStatus', 'CytoscapeConnectionClass',
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              cytoscapeMemoryStatus(base.url = loc.base.url)
          }) 

#------------------------------------------------------------------------------------------------------------------------
#' Free Up Unused Memory for Cytoscape
#' 
#' @description Manually call Java's garbage collection \code{System.gc()} to free up unused memory. 
#' This process happens automatically, but may be useful to call explicitly for testing or evaluation purposes. 
#' @param base.url (optional) URL prefix for CyREST calls
#' @return status message
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeFreeMemory()
#' 
#' base.url <- 'http://localhost:1234/v1'
#' cytoscapeFreeMemory(base.url)
#' }
#' @export

#' @rdname cytoscapeFreeMemory
setMethod('cytoscapeFreeMemory','missing', 
          function() {
              cytoscapeFreeMemory(base.url=.defaultBaseUrl);
          })
#' @rdname cytoscapeFreeMemory
setMethod('cytoscapeFreeMemory', 'character',
          function(base.url) {
              conn.str <- paste(base.url, 'gc', sep="/")
              res <- GET(conn.str)
              if(res$status_code == 204) {
                  return("Unused memory freed up.")
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          })
# DEPRECATED
setMethod('cytoscapeFreeMemory', 'CytoscapeConnectionClass',
          function(base.url) {
              .Deprecated('character or default argument')
              loc.base.url <- paste(base.url@uri, base.url@api,sep="/")
              cytoscapeFreeMemory(base.url = loc.base.url)
          })




