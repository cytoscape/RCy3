#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1/collections?subsuid=sub.suid
setGeneric ('getCollectionList', function (obj, sub.title=NA) standardGeneric('getCollectionList'))

#http://localhost:1234/v1/collections/count 
setGeneric ('getCollectionCount', function (obj) standardGeneric('getCollectionCount'))

# ------------------------------------------------------------------------------
