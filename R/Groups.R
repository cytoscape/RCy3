# ==============================================================================
# Functions for working with GROUPS in Cytoscape.
#
# ------------------------------------------------------------------------------
# TODO: deleteGroup(title, group.name, base.url) http://localhost:1234/v1/networks/52/groups/count
# TODO: renameGroup (group.name, base.url) http://localhost:1234/v1/commands/group/rename 
# TODO: getGroupList (title, base.url) http://localhost:1234/v1/networks/52/groups
# TODO: getGroupCount (title, base.url) http://localhost:1234/v1/networks/52/groups/count

# ------------------------------------------------------------------------------
#' @title Create Group
#'
#' @description FUNCTION_DESCRIPTION
#' @param group.name DESCRIPTION
#' @param nodes List of node SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected nodes.
#' @param nodes.by.col name of node table column corresponding to provided nodes 
#' list. Default is 'SUID'.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' createGroup()
#' }
#' @export
createGroup <- function(group.name, nodes=NULL, nodes.by.col='SUID', 
                        network=NULL, base.url=.defaultBaseUrl){
    node.list <- .prepPostQueryLists(nodes, nodes.by.col)
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group create groupName="',group.name,'"',
                                          ' nodeList="',node.list,'"',
                                          ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Collapse Group
#'
#' @description FUNCTION_DESCRIPTION
#' @param groups (optional) List of group names or keywords: all, selected,
#' unselected. Default is the currently selected group.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' collapseGroup()
#' }
#' @export
collapseGroup <- function(groups=NULL, network=NULL, base.url=.defaultBaseUrl){
    group.list <- .prepPostQueryLists(groups)
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group collapse groupList="',group.list,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
    
}
# ------------------------------------------------------------------------------
#' @title Expand Group
#'
#' @description FUNCTION_DESCRIPTION
#' @param groups (optional) List of group names or keywords: all, selected,
#' unselected. Default is the currently selected group.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' expandGroup()
#' }
#' @export
expandGroup <- function(groups=NULL, network=NULL, base.url=.defaultBaseUrl){
    group.list <- .prepPostQueryLists(groups)
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group expand groupList="',group.list,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
    
}
