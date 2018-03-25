# ==============================================================================
# Functions for working with GROUPS in Cytoscape.
#
# ------------------------------------------------------------------------------
# TODO: deleteGroup(title, group.name, base.url) http://localhost:1234/v1/networks/52/groups/count
# TODO: renameGroup (group.name, base.url) http://localhost:1234/v1/commands/group/rename 
# TODO: getGroupList (title, base.url) http://localhost:1234/v1/networks/52/groups
# TODO: getGroupCount (title, base.url) http://localhost:1234/v1/networks/52/groups/count

# ------------------------------------------------------------------------------
#' @title Add to Group
#'
#' @description Adds the specified nodes and edges to the specified group.
#' @param group.name Specifies the name used to identify the group
#' @param nodes List of node SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected nodes.
#' @param nodes.by.col name of node table column corresponding to provided nodes 
#' list. Default is 'SUID'.
#' @param edges  List of edge SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected edges.
#' @param edges.by.col name of edge table column corresponding to provided edges 
#' list. Default is 'SUID'.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' AddToGroup()
#' }
#' @export
AddToGroup <- function(group.name, nodes=NULL, nodes.by.col='SUID', 
                        edges=NULL, edges.by.col='SUID', 
                        network=NULL, base.url=.defaultBaseUrl){
    
    if (length(nodes)==1 && nodes[1] %in% c('all','selected','unselected'))
        nodes.by.col = NULL
    
    node.list <- .prepPostQueryLists(nodes, nodes.by.col)
    
    if (length(edges)==1 && edges[1] %in% c('all','selected','unselected'))
        edges.by.col = NULL
    
    edge.list <- .prepPostQueryLists(edges, edges.by.col)
    
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group add groupName="',group.name,'"',
                        ' nodeList="',node.list,'"',
                        ' edgeList="',edge.list,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Collapse Group
#'
#' @description Replaces the representation of all of the nodes and edges in a group with a single node.
#' @param groups (optional) List of group names or keywords: all, selected,
#' unselected. Default is the currently selected group.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
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
#' @title Create Group
#'
#' @description Create a group from the specified nodes.
#' @param group.name Specifies the name used to identify the group
#' @param nodes List of node SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected nodes.
#' @param nodes.by.col name of node table column corresponding to provided nodes 
#' list. Default is 'SUID'.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Group SUID
#' @examples \donttest{
#' createGroup()
#' }
#' @export
createGroup <- function(group.name, nodes=NULL, nodes.by.col='SUID', 
                        network=NULL, base.url=.defaultBaseUrl){
    
    if (length(nodes)==1 && nodes[1] %in% c('all','selected','unselected'))
        nodes.by.col = NULL
    
    node.list <- .prepPostQueryLists(nodes, nodes.by.col)
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group create groupName="',group.name,'"',
                                          ' nodeList="',node.list,'"',
                                          ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
}


# ------------------------------------------------------------------------------
#' @title Expand Group
#'
#' @description Replaces the group node with member nodes for a set of groups.
#' @param groups (optional) List of group names or keywords: all, selected,
#' unselected. Default is the currently selected group.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
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

# ------------------------------------------------------------------------------
#' @title Get Group Information
#'
#' @description Retrieve information about a group by providing a network and the group node identifier.
#' @param group (optional) Group name or SUID.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Group information
#' @examples \donttest{
#' getGroupInfo('mygroup')
#' }
#' @export
getGroupInfo <- function(group, network=NULL, base.url=.defaultBaseUrl){
    group.suid <- .nodeNameToNodeSUID(group)
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group get node="SUID:',group.suid,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
    
}

# ------------------------------------------------------------------------------
#' @title List Groups
#'
#' @description Retrieve a list of all group SUIDs in a network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of group SUIDs
#' @examples \donttest{
#' listGroups()
#' }
#' @export
listGroups <- function(network=NULL, base.url=.defaultBaseUrl){
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group list',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
    
}

# ------------------------------------------------------------------------------
#' @title Remove from Group
#'
#' @description Removes the specified nodes and edges from the specified group.
#' @param group.name Specifies the name used to identify the group
#' @param nodes List of node SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected nodes.
#' @param nodes.by.col name of node table column corresponding to provided nodes 
#' list. Default is 'SUID'.
#' @param edges  List of edge SUIDs, names, other column values, or keyword: 
#' selected, unselected or all. Default is currently selected edges.
#' @param edges.by.col name of edge table column corresponding to provided edges 
#' list. Default is 'SUID'.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' RemoveFromGroup()
#' }
#' @export
RemoveFromGroup <- function(group.name, nodes=NULL, nodes.by.col='SUID', 
                       edges=NULL, edges.by.col='SUID', 
                       network=NULL, base.url=.defaultBaseUrl){
    
    if (length(nodes)==1 && nodes[1] %in% c('all','selected','unselected'))
        nodes.by.col = NULL
    
    node.list <- .prepPostQueryLists(nodes, nodes.by.col)
    
    if (length(edges)==1 && edges[1] %in% c('all','selected','unselected'))
        edges.by.col = NULL
    
    edge.list <- .prepPostQueryLists(edges, edges.by.col)
    
    net.suid <- getNetworkSuid(network)
    commandsPOST(paste0('group remove groupName="',group.name,'"',
                        ' nodeList="',node.list,'"',
                        ' edgeList="',edge.list,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Delete (or Ungroup) a Group
#'
#' @description Deletes one or more groups, while leaving member nodes intact.
#' @param groups (optional) List of group SUIDs, names, other column values or keywords: all, selected,
#' unselected. Default is the currently selected group.
#' @param nodes.by.col name of node table column corresponding to provided groups 
#' list. Default is 'SUID'.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' deleteGroup()
#' }
#' @details Note: group nodes are ungrouped but not deleted in Cytoscape version 3.6.1
#' @export
deleteGroup <- function(groups=NULL, groups.by.col='SUID',
                          network=NULL, base.url=.defaultBaseUrl){
    if (length(groups)==1 && groups[1] %in% c('all','selected','unselected'))
        groups.by.col = NULL
    
    group.list <- .prepPostQueryLists(groups, groups.by.col)
    net.suid <- getNetworkSuid(network)
    
    commandsPOST(paste0('group ungroup groupList="',group.list,'"',
                        ' network="SUID:',net.suid,'"'),
                 base.url = base.url)
    
}
