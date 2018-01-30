#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1/networks/52/groups/count
setGeneric ('createGroup', function (obj, title=NA, group.name, node.names) standardGeneric('createGroup'))

#http://localhost:1234/v1/networks/52/groups/count
setGeneric ('deleteGroup', function (obj, title=NA, group.name) standardGeneric('deleteGroup'))

#http://localhost:1234/v1/commands/group/rename 
setGeneric ('renameGroup', function (obj, group.name) standardGeneric('renameGroup'))

#http://localhost:1234/v1/networks/52/groups/count
setGeneric ('expandGroup', function (obj, title=NA, group.name) standardGeneric('expandGroup'))

#http://localhost:1234/v1/networks/52/groups/count
setGeneric ('collapseGroup', function (obj, title=NA, group.name) standardGeneric('collapseGroup'))

#http://localhost:1234/v1/networks/52/groups
setGeneric ('getGroupList', function (obj, title=NA) standardGeneric('getGroupList'))

#http://localhost:1234/v1/networks/52/groups/count
setGeneric ('getGroupCount', function (obj, title=NA) standardGeneric('getGroupCount'))
    
# ------------------------------------------------------------------------------
