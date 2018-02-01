
# ------------------------------------------------------------------------------
#setGeneric ('ping', 	 	          function (base.url) standardGeneric('ping'))
#setGeneric ('cytoscapeVersionInfo',   function (base.url) standardGeneric('cytoscapeVersionInfo'))
#setGeneric ('availableApiVersions',   function (base.url) standardGeneric('availableApiVersions'))
#setGeneric ('cytoscapeNumberOfCores', function (base.url) standardGeneric('cytoscapeNumberOfCores'))
#setGeneric ('cytoscapeMemoryStatus',  function (base.url) standardGeneric('cytoscapeMemoryStatus'))
#setGeneric ('cytoscapeFreeMemory',    function (base.url) standardGeneric('cytoscapeFreeMemory'))

#------------------------------------------------------------------------------------------------------------------------
#' Ping Cytoscape
#' 
#' @description Test the connection to Cytoscape via CyREST 
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return status message
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' ping()
#' # [1] "You are connected to Cytoscape!"
#' }
#' @export

ping<-function(base.url=.defaultBaseUrl) {
              conn.str <- paste(base.url, 'version', sep="/")
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  return("You are connected to Cytoscape!")
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }

#------------------------------------------------------------------------------------------------------------------------
#' Cytoscape and CyREST API Versions
#' 
#' @description Returns the versions of the current Cytoscape and CyREST API
#' @param base.url (optional) URL prefix for CyREST calls
#' @return \code{list} of versions
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeVersionInfo()
#'  # apiVersion cytoscapeVersion 
#'  #       "v1" "3.7.0-SNAPSHOT" 
#' }
#' @export
cytoscapeVersionInfo<-function(base.url=.defaultBaseUrl) {
              conn.str <- paste(base.url, 'version', sep="/")
              res <- GET(conn.str)
              versions <- fromJSON(rawToChar(res$content))
              if(length(versions[[1]]) == 0) {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
              return(versions)
          }

#------------------------------------------------------------------------------------------------------------------------
#' Available CyREST API Versions 
#' 
#' @description Get the list of available CyREST API versions 
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{list} of API versions as \code{character} strings, e.g., "v1"
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' availableApiVersions()
#' # [1] "v1"
#' }
#' @export
availableApiVersions<-function(base.url=.defaultBaseUrl) {
              uri <- strsplit(base.url,'/v')[[1]][1]
              res <- GET(uri)
              available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
              return(available.api.versions)
          }

#------------------------------------------------------------------------------------------------------------------------
#' Number of Processors Available to Cytoscape
#' 
#' @description Returns the processor resources of the server running Cytoscape
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{numeric} value
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeNumberOfCores()
#' # [1] 8
#' }
#' @export
cytoscapeNumberOfCores<-function(base.url=.defaultBaseUrl) {
              conn.str <- paste(base.url)
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  status <- fromJSON(rawToChar(res$content))
                  return(status$numberOfCores)
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }

#------------------------------------------------------------------------------------------------------------------------
#' Memory Available to Cytoscape
#' 
#' @description Returns the memory resources of the server running Cytoscape
#' @param base.url (optional) URL prefix for CyREST calls
#' @return A \code{list} of \code{numeric} values
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeMemoryStatus()
#' #  usedMemory  freeMemory totalMemory   maxMemory 
#' #         181        2624        2805       13653 
#' }
#' @export
cytoscapeMemoryStatus<-function(base.url=.defaultBaseUrl) {
              conn.str <- paste(base.url)
              res <- GET(conn.str)
              if(res$status_code == 200) {
                  status <- fromJSON(rawToChar(res$content))
                  return(status$memoryStatus)
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }

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
#' # [1] "Unused memory freed up."
#' }
#' @export
cytoscapeFreeMemory<-function(base.url=.defaultBaseUrl) {
              conn.str <- paste(base.url, 'gc', sep="/")
              res <- GET(conn.str)
              if(res$status_code == 204) {
                  return("Unused memory freed up.")
              } else {
                  write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
                  stop()
              }
          }


