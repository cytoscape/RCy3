# ==============================================================================
# Functions for getting information about network COLLECTIONS.
# ------------------------------------------------------------------------------
#' @title Get Collection List
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getCollectionList()
#' }
#' @export
getCollectionList <- function(base.url=.defaultBaseUrl){
    res <- cyrestGET('collections', base.url)
    coll.names <- lapply(res, function(x) getCollectionName(x, base.url))
    return(unlist(coll.names))
}

# ------------------------------------------------------------------------------
#' @title Get Collection Suid
#'
#' @description FUNCTION_DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getCollectionSuid()
#' }
#' @export
getCollectionSuid <- function(network=NULL, base.url=.defaultBaseUrl){
    net.suid <- getNetworkSuid(network, base.url)
    res <- cyrestGET('collections',
                     parameters = list(subsuid=net.suid),
                     base.url = base.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title Get Collection Name
#'
#' @description FUNCTION_DESCRIPTION
#' @param collection.suid DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getCollectionName()
#' }
#' @export
getCollectionName <- function(collection.suid=NULL, base.url=.defaultBaseUrl){
    if(is.null(collection.suid))
        collection.suid <- getCollectionSuid()
    res <- cyrestGET(paste('collections',collection.suid,'tables/default', sep="/"),
                     base.url = base.url)
    return(unname(unlist(res$rows)['name']))
}


# ------------------------------------------------------------------------------
#' @title Get Collection Networks
#'
#' @description FUNCTION_DESCRIPTION
#' @param collection.suid DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getCollectionNetworks()
#' }
#' @export
getCollectionNetworks <- function(collection.suid=NULL, base.url=.defaultBaseUrl){
    if(is.null(collection.suid))
        collection.suid <- getCollectionSuid()
    res <- cyrestGET(paste('collections',collection.suid,'subnetworks', sep="/"),
                     base.url = base.url)
    return(res)
}
