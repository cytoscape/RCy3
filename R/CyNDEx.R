# ==============================================================================
# Functions for communicating with NDEx from within Cytoscape.
# ------------------------------------------------------------------------------
#' @title Import Network From NDEx
#'
#' @description Import a network from the NDEx database into Cytoscape.
#' @param ndex.id Network \code{externalId} provided by NDEx. This is not the 
#' same as a Cytoscape SUID.
#' @param username (optional) NDEx account username; required for private content
#' @param password (optional) NDEx account password; required for private content
#' @param accessKey (optional) NDEx accessKey; alternate acccess to private content
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return (integer) SUID of imported network
#' @examples \donttest{
#' importNetworkFromNDEx(ndex.id)
#' }
#' @export
importNetworkFromNDEx <- function (ndex.id, username=NULL, password=NULL, 
                                   accessKey=NULL, base.url = .cyndexBaseUrl){
    ndex.body <- list(serverUrl="http://ndexbio.org/v2",
                      uuid=ndex.id)
    if(!is.null(username))
        ndex.body[['username']] <- username
    if(!is.null(password))
        ndex.body[['password']] <- password
    if(!is.null(accessKey))
        ndex.body[['accessKey']] <- accessKey
    
    res <- cyrestPOST('networks',
                       body = ndex.body,
                       base.url = base.url)
    return(res$data$suid)
}

# ------------------------------------------------------------------------------
#' @title Export Network To NDEx
#'
#' @description Send a copy of a Cytoscape network to NDEx as a new submission.
#' @param username NDEx account username
#' @param password NDEx account password
#' @param isPublic (Boolean) Whether to make the network publicly 
#' accessible at NDEx. 
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param metadata (optional) A list of structured information describing the network
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return NDEx identifier (\code{externalId}) for new submission
#' @examples \donttest{
#' exportNetworkToNDEx("user", "pass", TRUE)
#' }
#' @export
exportNetworkToNDEx <- function(username, password, isPublic, 
                                network=NULL, metadata=NULL, 
                                base.url = .cyndexBaseUrl){
    
    suid <- getNetworkSuid(network)
    
    res <- cyrestPOST(paste('networks',suid,sep = '/'),
                      body = list(serverUrl="http://ndexbio.org/v2",
                                  username=username,
                                  password=password,
                                  metadata=metadata,
                                  isPublic=isPublic),
                      base.url = base.url)
    return(res$data$uuid)
}

# ------------------------------------------------------------------------------
#' @title Update Network In NDEx
#'
#' @description Update an existing network in NDEx, given a previously 
#' assoicaiated Cytoscape network, e.g., previously exported to NDEx or imported
#' from NDEx. 
#' @param username NDEx account username
#' @param password NDEx account password
#' @param isPublic (Boolean) Whether to make the network publicly 
#' accessible at NDEx.
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param metadata (optional) A list of structured information describing the network
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return NDEx identifier (\code{externalId}) for the updated submission
#' @examples \donttest{
#' updateNetworkInNDEx("user", "pass", TRUE)
#' }
#' @export
updateNetworkInNDEx <- function(username, password, isPublic, 
                                network=NULL, metadata=NULL, 
                                 base.url = .cyndexBaseUrl){
    
    suid <- getNetworkSuid(network)
    
    res <- cyrestPUT(paste('networks',suid,sep = '/'),
                      body = list(serverUrl="http://ndexbio.org/v2",
                                  username=username,
                                  password=password,
                                  metadata=metadata,
                                  isPublic=isPublic),
                      base.url = base.url)
    return(res$data$uuid)
}

# ------------------------------------------------------------------------------
#' @title Get Network NDEx Id
#'
#' @description Retrieve the NDEx externalId for a Cytoscape network, presuming
#' it has already been exported to NDEx.
#' @details If the Cytoscape network is not associated with an NDEx network, the
#' return value will be NULL.
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return NDEx identifier (\code{externalId}) or NULL
#' @examples \donttest{
#' getNetworkNDExId()
#' }
#' @export
getNetworkNDExId <- function(network=NULL, base.url = .cyndexBaseUrl) {
    suid <- getNetworkSuid(network)
    res <- cyrestGET(paste('networks', suid,sep = '/'),
                     base.url = base.url)
    
    return(res$data$members[[1]]$uuid)
}

