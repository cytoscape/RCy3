# ==============================================================================
# Functions for NETWORK management and retrieving information on networks, nodes
# and edges. Includes all functions that result in the creation of a new network
# in Cytoscape, in addition to funcitons that extract network models into
# other useful objects. 
#
# I. General network functions
# II. General node functions
# III. General edge functions
# IV. Network creation
# V. Network extraction
# VI. Internal functions
# 
# Note: Go to NetworkSelection.R for all selection-related functions
# 
# ==============================================================================
# I. General network functions
# ------------------------------------------------------------------------------
#' @title Set current network
#'
#' @description Selects the given network as "current"
#' @param network name or suid of the network that you want set as current
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @export
#' @examples
#' \donttest{
#' setCurrentNetwork('MyNetwork')
#' }
setCurrentNetwork <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid = getNetworkSuid(network)
    cmd <- paste0('network set current network=SUID:"', suid, '"')
    commandsPOST(cmd, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Rename a network
#'
#' @description Sets a new name for this network
#' @details Duplicate network names are not allowed
#' @param new.title New name for the network
#' @param network name or suid of the network that you want to rename; default is "current" network
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @author Alexander Pico, Julia Gustavsen
#' @examples \dontrun{
#' renameNetwork("renamed network")
#' }
#' @export
renameNetwork <-  function(new.title,
                           network = NULL,
                           base.url = .defaultBaseUrl) {
    old.suid = getNetworkSuid(network)
    cmd <-
        paste0('network rename name="',
               new.title,
               '" sourceNetwork=SUID:"',
               old.suid,
               '"')
    res <- commandsPOST(cmd, base.url = base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @title Get the number of Cytoscape networks
#'
#' @description Returns the number of Cytoscape networks in the current Cytoscape session
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{numeric}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \dontrun{
#' getNetworkCount()
#' # 3
#' }
#' @export
getNetworkCount <- function(base.url = .defaultBaseUrl) {
    res <- cyrestGET('networks/count', base.url = base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' @title Get the name of a network
#'
#' @description Retrieve the title of a network
#' @param suid SUID of the network; default is current network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return network name
#' @export
#' @examples
#' \donttest{
#' getNetworkName()
#' getNetworkName(1111)
#' }
#
# Dev Notes: together with getNetworkSuid, this function attempts to handle all 
# of the multiple ways we support network referencing (e.g., title, SUID, 
# 'current', and NULL). These functions are then used by all other functions
# that take a "network" argument.
# 
getNetworkName <- function(suid = NULL, base.url = .defaultBaseUrl) {
    if (is.character(suid)) {
        #title provided
        if (suid == 'current') {
            network.suid = getNetworkSuid()
        } else {
            net.names <- getNetworkList(base.url = base.url)
            if (suid %in% net.names) {
                return(suid)
            } else {
                stop(paste0("Network does not exist: ", suid))
            }
        }
    } else if (is.numeric(suid)) {
        #suid provided
        network.suid = suid
    } else {
        #use current network
        network.suid = getNetworkSuid()
    }
    
    res <-
        cyrestGET('networks.names',
                  list(
                      column = "suid",
                      query = network.suid,
                      base.url = base.url
                  ))
    network.name <- unname(res)[[1]]$name
    return(network.name)
}

# ------------------------------------------------------------------------------
#' @title Get the SUID of a network
#'
#' @description Retrieve the SUID of a network
#' @param title Name of the network; default is "current" network
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return (\code{numeric}) Network suid
#' @author Alexander Pico
#' @examples
#' \donttest{
#' getNetworkSuid()
#' getNetworkSuid("myNetwork")
#' # 80
#' }
#' @export
#
# Dev Notes: together with getNetworkName, this function attempts to handle all 
# of the multiple ways we support network referencing (e.g., title, SUID, 
# 'current', and NULL). These functions are then used by all other functions
# that take a "network" argument.
# 
getNetworkSuid <- function(title = NULL, base.url = .defaultBaseUrl) {
    if (is.character(title)) {
        #title provided
        if (title == 'current') {
            network.title = title
        } else {
            net.names <- getNetworkList(base.url = base.url)
            if (title %in% net.names) {
                network.title = title
            } else {
                stop(paste0("Network does not exist: ", title))
            }
        }
    } else if (is.numeric(title)) {
        #suid provided
        net.suids <- cyrestGET('networks', base.url = base.url)
        if (title %in% net.suids) {
            return(title)
        } else {
            stop(paste0("Network does not exist: ", title))
        }
    } else {
        #use current network
        network.title = 'current'
    }
    
    cmd <-
        paste0(
            'network get attribute network="',
            network.title,
            '" namespace="default" columnList="SUID"'
        )
    res <- commandsPOST(cmd, base.url = base.url)
    suid <- gsub("\\{SUID:|\\}", "", res)
    return(as.numeric(suid))
}

# ------------------------------------------------------------------------------
#' @title Get the list of Cytoscape networks
#'
#' @description Returns the list of Cytoscape network names in the current Cytoscape session
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \dontrun{
#' getNetworkList()
#' # 3
#' }
#' @export
getNetworkList <- function(base.url = .defaultBaseUrl) {
    if (getNetworkCount(base.url) == 0) {
        return(c())
    }
    cy.networks.SUIDs <- cyrestGET('networks', base.url = base.url)
    cy.networks.names = c()
    for (suid in cy.networks.SUIDs)	{
        res <-
            cyrestGET(paste("networks", as.character(suid), sep = "/"), base.url = base.url)
        net.name <- res$data$name
        cy.networks.names <- c(cy.networks.names, net.name)
    }
    return(cy.networks.names)
}

# ------------------------------------------------------------------------------
#' @title Export Network 
#'
#' @description Export a network to one of mulitple file formats
#' @param filename DESCRIPTION
#' @param type File type. CX, CYJS, GraphML, NNF, SIF, XGMML (case sensitive)
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None.
#' @examples \donttest{
#' exportNetwork('/path/filename','SIF')
#' }
#' @export
exportNetwork <- function (filename, type, base.url = .defaultBaseUrl) {
    if (!file.exists(filename)) {
        type = toupper(type)
        if (type == 'CYS') {
            print('Saving entire session as a CYS file.')
            saveSession(filename = filename, base.url = base.url)
        }
        else {
            #e.g., CX, CYJS, GraphML, NNF, SIF, XGMML
            if (type == "GRAPHML")
                #fix case for exceptions
                type = 'GraphML'
            commandsPOST(paste0('network export options=',
                                type, ' OutputFile="', filename, '"'),
                         base.url = base.url)
        }
    } else{
        write (sprintf ('choose another filename. File exists: %s', filename),
               stderr ())
    }
}

# ------------------------------------------------------------------------------
#' @title Delete Network
#'
#' @description FUNCTION_DESCRIPTION
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' deleteNetwork()
#' }
#' @export
deleteNetwork <- function (network = NULL, base.url = .defaultBaseUrl) {
    suid = getNetworkSuid(network)
    res = cyrestDELETE(paste("networks", suid, sep = "/"), base.url = base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @title Delete All Networks
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' deleteAllNetworks()
#' }
#' @export
deleteAllNetworks <- function (base.url = .defaultBaseUrl) {
    res = cyrestDELETE("networks", base.url = base.url)
    invisible(res)
}

# ==============================================================================
# II. General node functions
# ------------------------------------------------------------------------------
#' Get list of nodes neighboring provided list
#'
#' @description Returns a non-redundan list of first
#' neighbors of the supplied list of nodes.
#' @param node.names A \code{list} of node names from the \code{name} column of the \code{node table}
#' @param as.nested.list \code{logical} Whether to return lists of neighbors per query node
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A list of unique node names, optionally nested per query node name.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @seealso
#' selectNodes
#' selectFirstNeighbors
#' @examples \donttest{
#' getFirstNeighbors()
#' }
#' @export
getFirstNeighbors <-
    function (node.names,
              as.nested.list = FALSE,
              network = NULL,
              base.url = .defaultBaseUrl) {
        if (length (node.names) == 0)
            return()
        
        net.SUID = getNetworkSuid(network)
        neighbor.names <- c()
        
        for (node.name in node.names) {
            # get first neighbors for each node
            node.SUID = .nodeNameToNodeSUID(node.name, net.SUID, base.url)
            first.neighbors.SUIDs <- cyrestGET(
                paste(
                    "networks",
                    net.SUID,
                    "nodes",
                    as.character(node.SUID),
                    "neighbors",
                    sep = "/"
                ),
                base.url = base.url
            )
            first.neighbors.names <-
                .nodeSUIDToNodeName(first.neighbors.SUIDs, net.SUID, base.url)
            
            if (as.nested.list) {
                neighbor.names <-
                    append(neighbor.names, list(c(
                        node.name, list(first.neighbors.names)
                    )))
            } else {
                neighbor.names <- c(neighbor.names, first.neighbors.names)
                neighbor.names <-
                    unique(unlist(neighbor.names, use.names = FALSE))
            }
        }
        return (neighbor.names)
    }

# ------------------------------------------------------------------------------
#' @title Add CyNodes
#'
#' @description Add one or more nodes to a Cytoscape network.
#' @param node.names A \code{list} of node names
#' @param skip.duplicate.names Skip adding a node if a node with the same name is already in
#' the network. If \code{FALSE} then a duplicate node (with a unique SUID) will be added. Default
#' is \code{TRUE}.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of \code{named lists} of name and SUID for each node added.
#' @examples \donttest{
#' addCyNodes()
#' }
#' @importFrom BiocGenerics setdiff
#' @export
addCyNodes <- function(node.names,
                       skip.duplicate.names = TRUE,
                       network = NULL,
                       base.url = .defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    
    if (skip.duplicate.names)
        node.names <-
            setdiff(node.names, getAllNodes(net.suid, base.url))
    
    cyrestPOST(
        paste("networks", net.suid, "nodes", sep = "/"),
        body = node.names,
        base.url = base.url
    )
}

# ------------------------------------------------------------------------------
#' @title Get Node Count
#'
#' @description Reports the number of nodes in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{numeric}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getNodeCount()
#' }
#' @export
getNodeCount <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <-
        cyrestGET(paste("networks", net.SUID, "nodes/count", sep = "/"),
                  base.url = base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' @title Get All Nodes
#'
#' @description Retrieve the names of all the nodes in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of node names
#' @examples \donttest{
#' getAllNodes()
#' }
#' @export
getAllNodes <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    n.count <- getNodeCount(net.SUID, base.url)
    if (n.count == 0) {
        return()
    }
    res <- cyrestGET(
        paste(
            "networks",
            net.SUID,
            "tables/defaultnode/columns/name",
            sep = "/"
        ),
        base.url = base.url
    )
    return(res$values)
}

# ==============================================================================
# III. General edge functions
# ------------------------------------------------------------------------------
#' @title Add CyEdges
#'
#' @description Add one or more edges to a Cytoscape network by listing source and 
#' target node pairs.
#' @param source.target.list A \code{list} (or \code{list of lists}) of source and target node pairs
#' @param edgeType The type of interaction. Default is 'interacts with'.
#' @param directed \code{boolean} for whether interactions are directed. Default is \code{FALSE}.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of \code{named lists} of SUID, source and target for each edge added.
#' @examples \donttest{
#' addCyEdges(c('sourceNode','targetNode'))
#' addCyEdges(list(c('s1','t1'),c('s2','t2')))
#' }
#' @importFrom stats setNames
#' @export
addCyEdges <-
    function (source.target.list,
              edgeType = 'interacts with',
              directed = FALSE,
              network = NULL,
              base.url = .defaultBaseUrl) {
        net.suid <- getNetworkSuid(network)
        
        # swap with node suids
        if (length(unlist(source.target.list)) > 2) {
            # list of lists
            edge.suid.list <- lapply(source.target.list, function(x)
                lapply(x, function(y)
                    .nodeNameToNodeSUID(y, net.suid, base.url)))
            
        } else {
            # just single edge pair
            edge.suid.list <- list(lapply(source.target.list,
                                          function(y)
                                              .nodeNameToNodeSUID(y, net.suid, base.url)))
        }
        
        # check for unique node name<->suid mappings
        max.mapping <- lapply(edge.suid.list, function(x)
            lapply(x, function(y)
                max(length(y))))
        if (as.integer(max(unlist(max.mapping))) > 1) {
            write(
                'RCy3::addCyEdges, more than one node found for a given source or target node name. No edges added.',
                stderr()
            )
            return()
        }
        
        # add other fields
        edge.data <- lapply(edge.suid.list,
                            function(y)
                                c(
                                    setNames(as.list(unlist(y)), c("source", "target")),
                                    directed = directed,
                                    interaction = edgeType
                                ))
        
        cyrestPOST(
            paste("networks", net.suid, "edges", sep = "/"),
            body = edge.data,
            base.url = base.url
        )
    }

# ------------------------------------------------------------------------------
#' @title Get Edge Count
#'
#' @description Reports the number of the edges in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{numeric}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getEdgeCount()
#' }
#' @export
getEdgeCount <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <-
        cyrestGET(paste("networks", net.SUID, "edges/count", sep = "/"),
                  base.url = base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' @title Get All Edges
#'
#' @description Retrieve the names of all the edges in the network.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return \code{list} of node edges
#' @examples \donttest{
#' getAllEdges()
#' }
#' @export
getAllEdges <- function(network = NULL, base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    e.count <- getEdgeCount(net.SUID, base.url)
    if (e.count == 0) {
        return()
    }
    res <- cyrestGET(
        paste(
            "networks",
            net.SUID,
            "tables/defaultedge/columns/name",
            sep = "/"
        ),
        base.url = base.url
    )
    return(res$values)
}

# ==============================================================================
# IV. Network creation
# ------------------------------------------------------------------------------
#' @title Clone a Cytoscape Network
#'
#' @description Makes a copy of a Cytoscape Network with all of its edges and nodes.
#' @param network name or suid of the network you want to clone; default is "current" network
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return The \code{suid} of the new network
#' @examples \dontrun{
#' cloneNetwork("cloned network")
#' }
#' @author Alexander Pico, Julia Gustavsen
#' @export

cloneNetwork <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid = getNetworkSuid(network)
    cmd <- paste0('network clone network=SUID:"', suid, '"')
    res <- commandsPOST(cmd, base.url = base.url)
    return(res['network'])
}

# ------------------------------------------------------------------------------
#' @title Create subnetwork from existing network
#'
#' @description Copies a subset of nodes and edges into a newly created subnetwork.
#' @details If you spe@param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.cify both nodes and edges, the resulting subset will be the union of those sets.
#' Typical usage only requires specifying either nodes or edges. Note that selected nodes will bring
#' along their connecting edges by default (see exclude.edges arg) and selected edges will always
#' bring along their source and target nodes.
#' @param nodes list of node names or keyword: selected, unselected or all
#' @param nodes.by.col name of node table column corresponding to provided nodes list; default is 'name'
#' @param edges list of edge names or keyword: selected, unselected or all
#' @param edges.by.col name of edge table column corresponding to provided edges list; default is 'name'
#' @param exclude.edges (boolean) whether to exclude connecting edges; default is FALSE
#' @param subnetwork.name name of new subnetwork to be created;
#' default is to add a numbered suffix to source network name
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return SUID of new subnetwork
#' @export
#' @examples
#' \donttest{
#' createSubnetwork("selected")
#' createSubnetwork("selected",subnetwork.name="mySubnetwork")
#' createSubnetwork(c("node 1","node 2","node 3"))
#' createSubnetwork(c("AKT1","TP53","PIK3CA"),"display name")
#' createSubnetwork(edges="all") #subnetwork of all connected nodes
#' }
createSubnetwork <-
    function(nodes,
             nodes.by.col = 'name',
             edges,
             edges.by.col = 'name',
             exclude.edges = 'F',
             subnetwork.name,
             network = NULL,
             base.url = 'http://localhost:1234/v1') {
        title = getNetworkName(network, base.url)
        
        if (exclude.edges) {
            exclude.edges = "true"
        } else {
            exclude.edges = "false"
        }
        
        json_sub = NULL
        json_sub$source = title
        json_sub$excludeEdges = exclude.edges
        
        node.str = NULL
        if (missing(nodes)) {
            json_sub$nodeList = "selected" #need something here for edge selections to work
        } else {
            if (!nodes[1] %in% c('all', 'selected', 'unselected')) {
                for (n in nodes) {
                    if (is.null(node.str))
                        node.str = paste(nodes.by.col, n, sep = ":")
                    else
                        node.str = paste(node.str,
                                         paste(nodes.by.col, n, sep = ":"),
                                         sep = ",")
                }
            } else {
                node.str = nodes
            }
            json_sub$nodeList = node.str
        }
        
        edge.str = NULL
        if (!missing(edges)) {
            if (!edges[1] %in% c('all', 'selected', 'unselected')) {
                for (e in edges) {
                    if (is.null(edge.str))
                        edge.str = paste(edges.by.col, e, sep = ":")
                    else
                        edge.str = paste(edge.str,
                                         paste(edges.by.col, e, sep = ":"),
                                         sep = ",")
                }
            } else {
                edge.str = edges
            }
            json_sub$edgeList = edge.str
        }
        
        subnetwork.arg = NULL
        if (!missing(subnetwork.name)) {
            json_sub$networkName = subnetwork.name
        }
        
        sub <- toJSON(as.list(json_sub))
        url <-
            sprintf("%s/commands/network/create", base.url, sep = "") ##TODO swap with commandsPOST (POST)?
        response <-
            POST(
                url = url,
                body = sub,
                encode = "json",
                content_type_json()
            )
        subnetwork.suid = unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
        #cat(sprintf("Subnetwork SUID is : %i \n", subnetwork.suid))
        return(subnetwork.suid)
    }

# ------------------------------------------------------------------------------
#' @title Create a Cytoscape network from an igraph network
#'
#' @description Takes an igraph network and generates data frames for nodes and edges to
#' send to the createNetwork function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Vertices and edges from the igraph network will be translated into nodes and edges
#' in Cytoscape. Associated attributes will also be passed to Cytoscape as node and edge table columns.
#' @param igraph (igraph) igraph network object
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON(); see createNetwork
#' @return (int) network SUID
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetworkFromDataFrames, createIgraphFromNetwork
#' @importFrom igraph as_data_frame
#' @importFrom BiocGenerics colnames
#' @export
createNetworkFromIgraph <- function(igraph,
                                    new.title = "MyNetwork",
                                    collection.title = "myNetworkCollection",
                                    return.graph = FALSE,
                                    base.url = .defaultBaseUrl,
                                    ...) {
    #extract dataframes
    igedges = as_data_frame(igraph, what = "edges")
    ignodes = as_data_frame(igraph, what = "vertices")
    
    #setup columns for Cytoscape import
    ignodes$id <- row.names(ignodes)
    colnames(igedges)[colnames(igedges) == "from"] <- "source"
    colnames(igedges)[colnames(igedges) == "to"] <- "target"
    
    #ship
    createNetworkFromDataFrames(ignodes, igedges, new.title, collection.title, base.url)
}

# ------------------------------------------------------------------------------
#' @title createNetworkFromGraph
#'
#' @description Creates a Cytoscape network from a Bioconductor graph.
#' @return A Bioconductor graph object.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' createNetworkFromGraph(g, "myNetwork")
#' }
#' @export
createNetworkFromGraph <- function (graph,
                                    title = NULL,
                                    base.url = .defaultBaseUrl) {
    #TODO
    
    # #    # add a label to each node if not already present. default label is the node name, the node ID    	
    # if (is.classic.graph(graph)){
    #     if (edgemode(graph) == 'undirected') {
    #         graph = remove.redundancies.in.undirected.graph(graph) #AP: not sure this is needed anymore...
    #     }
    # }
    # # are all node attributes properly initialized?
    # node.attributes = noa.names(graph)
    # if (length(node.attributes) > 0) {
    #     check.list = list()
    #     for (node.attribute in node.attributes) {
    #         check.list[[node.attribute]] = properlyInitializedNodeAttribute(graph, node.attribute)
    #     }
    #     uninitialized.attributes = which(check.list == FALSE)
    #     if (length(uninitialized.attributes) > 0) {
    #         write(sprintf("%d uninitialized node attribute/s", length(uninitialized.attributes)), stderr())
    #         return()
    #     }
    # } # if node.attributes
    # 
    # # are all edge attributes properly initialized?
    # edge.attributes = eda.names(graph)
    # if (length(edge.attributes) > 0) {
    #     check.list = list()
    #     for (edge.attribute in edge.attributes) {
    #         check.list[[edge.attribute]] = properlyInitializedEdgeAttribute(graph, edge.attribute)
    #     }
    #     uninitialized.attributes = which(check.list == FALSE)
    #     if (length(uninitialized.attributes) > 0) {
    #         write(sprintf("%d uninitialized edge attribute/s", length(uninitialized.attributes)), stderr())
    #         return()
    #     }
    # } # if edge.attributes
    # 
    # if (!'label' %in% noa.names(graph)) {
    #     write('nodes have no label attribute -- adding default labels', stderr())
    #     graph = initNodeAttribute(graph, 'label', 'char', 'noLabel')
    #     if (length(nodes(graph) > 0)) {
    #         nodeData(graph, nodes(graph), 'label') = nodes(graph) # nodes(graph) returns strings
    #     }
    # }
    # 
    # # create new 'CytoscapeWindow' object
    # cw = new('CytoscapeWindowClass', title=title, graph=graph, uri=uri, api=api,
    #          collectTimings=collectTimings, node.suid.name.dict = list(), edge.node.suid.name.dict=list())
    # 
    # if (create.window) {
    #     cw@suid = sendNetworkFromGraph(cw)
    # }
    # 
}

# ------------------------------------------------------------------------------
#' @title Create a network from data frames
#'
#' @description Takes data frames for nodes and edges, as well as naming parameters to
#' generate the JSON data format required by the "networks" POST operation via CyREST.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details NODES should contain a column named: id. This name can be overridden by
#' the arg: node.id.list. Additional columns are loaded as node attributes.
#' EDGES should contain columns named: source, target and interaction. These names can be overridden by
#' args: source.id.list, target.id.list, interaction.type.list. Additional columns
#' are loaded as edge attributes. The 'interaction' list can contain a single
#' value to apply to all rows; and if excluded altogether, the interaction type
#' wiil be set to "interacts with". NOTE: attribute values of types (num) and (int) will be imported
#' as (Double); (chr) as (String); and (logical) as (Boolean).
#' @param nodes (data.frame) see details and examples below; default NULL to derive nodes from edge sources and targets
#' @param edges (data.frame) see details and examples below; default NULL for disconnected set of nodes
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON()
#' @return (int) network SUID
#' @examples
#' \donttest{
#' nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
#'            group=c("A","A","B","B"), # optional
#'            stringsAsFactors=FALSE)
#' edges <- data.frame(source=c("node 0","node 0","node 0","node 2"),
#'            target=c("node 1","node 2","node 3","node 3"),
#'            interaction=c("inhibits","interacts","activates","interacts"),  # optional
#'            weight=c(5,3,5,9), # optional
#'            stringsAsFactors=FALSE)
#'
#' createNetworkFromDataFrames(nodes,edges)
#' }
#' @export
createNetworkFromDataFrames <-
    function(nodes = NULL,
             edges = NULL,
             new.title = "MyNetwork",
             collection.title = "MyNetworkCollection",
             base.url = .defaultBaseUrl,
             ...) {
        #defining variable names to be used globally later on (to avoid devtools::check() NOTES)
        RCy3.CreateNetworkFromDataFrames.temp.global.counter <- NULL
        RCy3.CreateNetworkFromDataFrames.temp.global.size <- NULL
        RCy3.CreateNetworkFromDataFrames.temp.global.json_set <- NULL
        
        if (is.null(nodes)) {
            if (!is.null(edges)) {
                nodes = data.frame(
                    id = c(edges$source, edges$target),
                    stringsAsFactors = FALSE
                )
            } else
                return("Create Network Failed: Must provide either nodes or edges")
        }
        
        json_nodes <- .nodeSet2JSON(nodes, ...)
        # cleanup global environment variables (which can be quite large)
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter,
               envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.size,
               envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set,
               envir = globalenv())
        
        json_edges <- c()
        
        if (!is.null(edges)) {
            json_edges <- .edgeSet2JSON(edges, ...)
            # cleanup global environment variables (which can be quite large)
            remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter,
                   envir = globalenv())
            remove(RCy3.CreateNetworkFromDataFrames.temp.global.size,
                   envir = globalenv())
            remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set,
                   envir = globalenv())
        } else {
            json_edges <- "[]" #fake empty array
        }
        
        json_network <- list(data <- list(name = new.title),
                             elements <- c(
                                 nodes = list(json_nodes),
                                 edges = list(json_edges)
                             ))
        
        network.suid <- cyrestPOST('networks',
                                   parameters = list(
                                       title = new.title, 
                                       collection = collection.title),
                                   body = json_network,
                                   base.url = base.url)
        
        if (is.numeric(network.suid))
            cat(sprintf("Network SUID is : %i \n", network.suid))
        else
            return(network.suid)
        
        cat("Applying default style\n")
        commandsPOST('vizmap apply styles="default"', base.url = base.url)
        
        cat(sprintf("Applying %s layout\n", invisible(
            commandsPOST('layout get preferred network="current"', base.url = base.url)
        )))
        commandsPOST('layout apply preferred networkSelected="current',
                     base.url = base.url)
        
        return(network.suid)
    }

# ==============================================================================
# V. Network extraction
# ------------------------------------------------------------------------------
#' @title Create an igraph network from a Cytoscape network
#'
#' @description Takes a Cytoscape network and generates data frames for vertices and edges to
#' send to the graph_from_data_frame function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Nodes and edges from the Cytoscape network will be translated into vertices and edges
#' in igraph. Associated table columns will also be passed to igraph as vertiex and edge attributes.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (igraph) an igraph network
#' @examples
#' \donttest{
#' createIgraphFromNetwork('myNetwork')
#' }
#' @seealso createNetworkFromDataFrames, createNetworkFromIgraph
#' @importFrom igraph graph_from_data_frame
#' @importFrom BiocGenerics do.call
#' @importFrom BiocGenerics cbind
#' @importFrom BiocGenerics colnames
#' @export
createIgraphFromNetwork <-
    function(network = NULL,
             base.url = .defaultBaseUrl) {
        suid = getNetworkSuid(network)
        #get dataframes
        cyedges <-
            getTableColumns('edge', network = suid, base.url = base.url)
        cynodes <-
            getTableColumns('node', network = suid, base.url = base.url)
        
        #check for source and target columns
        if (!"source" %in% colnames(cyedges) ||
            (!"target" %in% colnames(cyedges))) {
            st = data.frame(do.call('rbind', strsplit(cyedges$name, "\ \\(.*\\)\ ")))
            colnames(st) <- c("source", "target")
            cyedges <- cbind(st, cyedges)
        }
        
        #setup columns for igraph construction
        colnames(cyedges)[colnames(cyedges) == "source"] <- "from"
        colnames(cyedges)[colnames(cyedges) == "target"] <- "to"
        cyedges2 = cbind(cyedges[c("from", "to")], cyedges[, !(names(cyedges) %in% c("from", "to"))])
        cynodes2 = cbind(cynodes["name"], cynodes[, !(names(cynodes) == "name")])
        
        #ship
        graph_from_data_frame(cyedges2, directed = TRUE, vertices = cynodes2)
    }

# ------------------------------------------------------------------------------
#' @title createGraphFromNetwork
#'
#' @description Returns the Cytoscape network as a Bioconductor graph.
#' @param network (optional) Name or SUID of the network. Default is the "current" network active in Cytoscape.
#' @param base.url cyrest base url for communicating with cytoscape
#' @return A Bioconductor graph object.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{cw <- CytoscapeWindow('network', graph=makeSimpleGraph())
#' displayGraph(cw)
#' layoutNetwork(cw)
#' g.net1 <- createGraphFromNetwork(cw)
#'
#' cc <- CytoscapeConnection()
#' g.net2 <- createGraphFromNetwork(cc, 'network')
#'
#' g.net3 <- createGraphFromNetwork('network') #default connection
#'
#' g.net4 <- createGraphFromNetwork() #current network
#' }
#' @export
createGraphFromNetwork <-
    function (network = NULL, base.url = .defaultBaseUrl) {
        suid = getNetworkSuid(network)
        title = getNetworkName(network)
        
        if (!is.na(suid)) {
            res = cyrestGET(paste("networks", suid, sep = "/"), base.url)
            g = new("graphNEL", edgemode = 'directed') # create graph object
            g.nodes = res$elements$nodes
            # if there are no nodes in the graph received from Cytoscape, return an empty 'graphNEL' object
            if (length(g.nodes) == 0) {
                write(
                    sprintf(
                        "NOTICE in RCy3::createGraphFromNetwork():\n\t returning an empty 'graphNEL'"
                    ),
                    stderr()
                )
                return(g)
            }
            
            # else get the node names and add them to the R graph
            node.suid.name.dict = lapply(g.nodes, function(n) {
                list(name = n$data$name,
                     SUID = n$data$SUID)
            })
            g.node.names = sapply(node.suid.name.dict, function(n) {
                n$name
            })
            write(sprintf("\t received %d NODES from '%s'", length(g.nodes), title),
                  stderr())
            g = graph::addNode(g.node.names, g)
            write(sprintf(
                "\t - added %d nodes to the returned graph\n",
                length(g.node.names)
            ),
            stderr())
            
            # GET NODE ATTRIBUTES (if any)
            g = .copyNodeAttributesToGraph(node.suid.name.dict, suid, g)
            
            # Bioconductor's 'graph' edges require the 'edgeType' attribute, so its default value is assigned
            g = .initEdgeAttribute (g, 'edgeType', 'char', 'assoc')
            
            # GET GRAPH EDGES
            g.edges = request.res$elements$edges
            
            if (length(g.edges) > 0) {
                regex = ' *[\\(|\\)] *'
                write(sprintf(
                    "\n\t received %d EDGES from '%s'",
                    length(g.edges),
                    title
                ),
                stderr())
                
                edge.node.suid.name.dict = lapply(g.edges, function(e) {
                    list(name = e$data$name,
                         SUID = e$data$SUID)
                })
                g.edge.names = sapply(edge.node.suid.name.dict, function(e) {
                    e$name
                })
                edges.tokens = strsplit(g.edge.names, regex)
                source.nodes = unlist(lapply(edges.tokens, function(tokens)
                    tokens[1]))
                target.nodes = unlist(lapply(edges.tokens, function(tokens)
                    tokens[3]))
                edge.types = unlist(lapply(edges.tokens, function(tokens)
                    tokens[2]))
                write(sprintf(
                    '\t - adding %d edges to the returned graph\n',
                    length(edges.tokens)
                ),
                stderr())
                
                tryCatch({
                    g = addEdge(source.nodes, target.nodes, g)
                    edgeData(g, source.nodes, target.nodes, 'edgeType') = edge.types
                    
                    # GET EDGE ATTRIBUTES (if any)
                    g = .copyEdgeAttributesToGraph(edge.node.suid.name.dict, suid, g)
                },
                error = function(cond) {
                    write(
                        sprintf(
                            "ERROR in RCy3::createGraphFromNetwork(): '%s'",
                            cond
                        ),
                        stderr()
                    )
                    return(NA)
                })
                
                
            }
            
        } else {
            write(
                sprintf(
                    "ERROR in RCy3::createGraphFromNetwork():\n\t there is no graph with name '%s' in Cytoscape",
                    title
                ),
                stderr()
            )
            return(NA)
        }
        
        return(g)
    }
## END createGraphFromNetwork


# ==============================================================================
# VI. Internal functions
# 
# Dev Notes: Prefix internal functions with a '.'. Do not @export and in general
# skip royxgen docs for these functions, with the exception of @importFrom lines.
# ------------------------------------------------------------------------------
# @title Convert edges to JSON format needed for CyRest network creation
#
# @param edge_set (data.frame) Rows contain pairwise interactions.
# @param source.id.list (char) override default list name for source node ids
# @param target.id.list (char) override default list name for target node ids
# @param interaction.type.list (char) override default list name for interaction types
#' @importFrom BiocGenerics colnames
.edgeSet2JSON <- function(edge_set,
                          source.id.list = 'source',
                          target.id.list = 'target',
                          interaction.type.list = 'interaction',
                          ...) {
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <-
        0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <- 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set <-
        c()
    
    if (!(interaction.type.list %in% names(edge_set)))
        edge_set[, interaction.type.list] = rep('interacts with')
    
    computed_name <-
        paste(edge_set[, source.id.list],
              paste('(', edge_set[, interaction.type.list], ')', sep = ''),
              edge_set[, target.id.list],
              sep = " ")
    
    for (i in 1:dim(edge_set)[1]) {
        rest <- list()
        rest[["name"]] = computed_name[i]
        for (j in 1:dim(edge_set)[2]) {
            rest[[colnames(edge_set)[j]]] = edge_set[i, j]
        }
        current_edge = list("data" = rest)
        .FastAppendListGlobal(current_edge)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# ------------------------------------------------------------------------------
# @title Creates a table of nodes to CyREST JSON
#
# @param node.set (data.frame) each row is a node and columns contain node attributes
# @param node.id.list (char) override default list name for node ids
# Adapted from Ruth Isserlin's CellCellINteractions_utility_functions.R
#' @importFrom BiocGenerics colnames
.nodeSet2JSON <- function(node.set, node.id.list = 'id', ...) {
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <-
        0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <- 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set <-
        c()
    
    for (i in 1:dim(node.set)[1]) {
        rest <- list()
        for (j in 1:dim(node.set)[2]) {
            rest[[colnames(node.set)[j]]] = node.set[i, j]
        }
        current_node = list("data" = rest)
        .FastAppendListGlobal(current_node)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# ------------------------------------------------------------------------------
# @title FastAppendListGlobal
# 
# @description Appends lists at high performance using global variables explictly
# @details https://stackoverflow.com/questions/17046336/here-we-go-again-append-an-element-to-a-list-in-r
.FastAppendListGlobal <- function(item)
{
    if (.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter == .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size)
        length(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set) <-
            .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <-
            .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size * 2
    
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <-
        .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter + 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[[.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter]] <-
        item
}
