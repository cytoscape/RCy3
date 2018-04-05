# ==============================================================================
# Functions for checking CYTOSCAPE SYSTEM information, including versions
# memory usage and a function to free Java memory used by the Cytoscape session. 
# 
# ------------------------------------------------------------------------------
#' Ping Cytoscape
#' 
#' @description Tests the connection to Cytoscape via CyREST and verifies that
#' supported versions of Cytoscape and CyREST API are loaded.  
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return status message
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' cytoscapePing()
#' # [1] "You are connected to Cytoscape!"
#' }
#' @export
cytoscapePing<-function(base.url=.defaultBaseUrl) {
    .verifySupportedVersions(1,3.6)
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
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of versions
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeVersionInfo()
#'  # apiVersion cytoscapeVersion 
#'  #       "v1" "3.7.0-SNAPSHOT" 
#' }
#' @export
cytoscapeVersionInfo<-function(base.url=.defaultBaseUrl) {
    versions <- cyrestGET('version',base.url = base.url)
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
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of API versions as \code{character} strings, e.g., "v1"
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' cytoscapeApiVersions()
#' # [1] "v1"
#' }
#' @export
cytoscapeApiVersions<-function(base.url=.defaultBaseUrl) {
    uri <- strsplit(base.url,'/v')[[1]][1]
    res <- GET(uri)
    available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
    return(available.api.versions)
}

#------------------------------------------------------------------------------------------------------------------------
#' Number of Processors Available to Cytoscape
#' 
#' @description Returns the processor resources of the server running Cytoscape
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{numeric} value
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeNumberOfCores()
#' # [1] 8
#' }
#' @export
cytoscapeNumberOfCores<-function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(base.url=base.url)
    return(res$numberOfCores)
}

#------------------------------------------------------------------------------------------------------------------------
#' Memory Available to Cytoscape
#' 
#' @description Returns the memory resources of the server running Cytoscape
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of \code{numeric} values
#' @author Alexander Pico
#' @examples \donttest{
#' cytoscapeMemoryStatus()
#' #  usedMemory  freeMemory totalMemory   maxMemory 
#' #         181        2624        2805       13653 
#' }
#' @export
cytoscapeMemoryStatus<-function(base.url=.defaultBaseUrl) {
    res <- cyrestGET(base.url=base.url)
    return(res$memoryStatus)
}

#------------------------------------------------------------------------------------------------------------------------
#' Free Up Unused Memory for Cytoscape
#' 
#' @description Manually call Java's garbage collection \code{System.gc()} to free up unused memory. 
#' This process happens automatically, but may be useful to call explicitly for testing or evaluation purposes. 
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
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


