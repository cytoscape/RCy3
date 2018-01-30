#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R

#------ Package Variables ----------------------------------------------------------------------------------------------
.defaultBaseUrl <- 'http://localhost:1234/v1'

#------ Package Functions ----------------------------------------------------------------------------------------------
.classicGraphToNodePairTable = function (g)
{
    edges.g <- edges(g)
    edge.names = as.character(unlist(sapply(names(edges.g), function (a) {
        bs = edges.g[[a]]; 
        if (length (bs) > 0) paste (a, edges.g[[a]], sep='~') 
    })))
    # print(class(edge.names))
    # print(edge.names)
    pairs = strsplit (edge.names, '~')
    a = sapply (pairs, "[", 1)
    b = sapply (pairs, "[", 2)
    
    if ('edgeType' %in% eda.names (g)){
        edgeType = as.character (edgeData (g, from=a, to=b, attr='edgeType'))
    }else{
        edgeType = rep ('unspecified', length (a))
    }
    
    
    return (data.frame (source=a, target=b, edgeType=edgeType, stringsAsFactors=FALSE))
    
} # .classicGraphToNodePairTable
#------------------------------------------------------------------------------------------------------------------------
.multiGraphToNodePairTable = function (mg)
{
    edge.set.names = edgeSets (mg)
    
    template = list (source='', target='', edgeType='')
    tbl = data.frame (template, stringsAsFactors=FALSE)
    for (edge.set in edgeSets (mg)) {
        tilde.names = edgeNames (mg, edge.set)
        pairs = strsplit (tilde.names, '~')
        for (pair in pairs) {
            source.node = pair [1]
            target.node = pair [2]
            new.row = list (source=source.node, target=target.node, edgeType=edge.set)
            tbl = rbind (tbl, new.row)
        }
    } # for edge
    
    invisible (tbl [-1,])     
    
} # .multiGraphToNodePairTable

# ------------------------------------------------------------------------------
# the bioc graph 'edgeNames' function does not detect, distinguish or report 
# reciprocal edges 
# this is fixed here
.rcyEdgeNames = function(g) 
{
    nodes.list = edges(g)
    result = c()
    for(source.node in names(nodes.list)) {
        target.nodes = nodes.list[[source.node]]
        
        if(length(target.nodes) == 0) {
            next;
        }
        for(target.node in target.nodes) {
            tilde.edge.name = sprintf('%s~%s', source.node, target.node) 
            result = c(result, tilde.edge.name)
        } # for target.node
    } # for source.node
    
    return(result)
}

#------------------------------------------------------------------------------------------------------------------------
.getNovelEdges = function (g.old, g.new)
{
    if (length (edges (g.old)) == 0){
        gOld.edgeCount = 0
    } else {
        gOld.edgeCount = length (edgeNames (g.old))
    }
    
    if (length (edges (g.new)) == 0){
        gNew.edgeCount = 0
    } else{
        gNew.edgeCount = length (edgeNames (g.new))
    }
    
    if (gNew.edgeCount == 0){
        return (NA)
    }
    if (gOld.edgeCount == 0){
        return (cy2.edge.names (g.new))
    }
    
    old.edges = cy2.edge.names (g.old)
    new.edges = cy2.edge.names (g.new)
    novel.edges = setdiff (new.edges, old.edges)
    novel.edges.indices = match (novel.edges, as.character (new.edges))
    return (new.edges [novel.edges.indices])
    
    
} # .getNovelEdges

# ------------------------------------------------------------------------------
is.classic.graph = function(obj)
{
    obj.classes = is(obj)
    
    return ('graph' %in% obj.classes)
    
} # is.classic.graph

# ------------------------------------------------------------------------------
is.multiGraph = function(obj)
{
    obj.classes = is(obj)
    
    return('MultiGraph' %in% obj.classes)
    
} # is.multiGraph

# ------------------------------------------------------------------------------
# capitalizes the first letter of all words in a string
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    
    return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=""))
} ### END simpleCap


.isNotHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        write (sprintf ('Error. %s is not a valid hexadecimal color (has to begin with # and be 7 characters long).', color), stderr ())
        return(TRUE)
    }else{
        return(FALSE)
    }
}
# ------------------------------------------------------------------------------
# obtain every other value for vector : used to resolve CyREST bug with returning column values
obtainEveryOtherValue <- function(v) {
    return(v[c(TRUE, FALSE)])
}

# ------------------------------------------------------------------------------
getEdgeNamesAndSUIDS <- function(obj){
    # map edge names to edge SUIDs
    resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultedge", sep="/")
    request.res <- GET(url=resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    # get the row information from the edge table
    row.lst <- request.res[[6]]
    suids <- sapply(row.lst, '[[', "SUID")
    names <- sapply(row.lst, '[[', "name")
    edge.dict <- as.data.frame(cbind(names, suids))
    return (edge.dict)
}

# ------------------------------------------------------------------------------
findColumnType <- function(columnType){
    if (columnType=="double"){
        return("Double")
    } else if (columnType == "integer"){
        return("Integer")
    } else if (columnType == "logical"){
        return("Boolean")
    } else{
        return("String")
    }
} # findColumnType
# ------------------------------------------------------------------------------
.nodeNameToNodeSUID<-function(obj, node.names) {
    
    if(length(obj@node.suid.name.dict)==0){  # i.e., no obj dictionary
        dict <- getTableColumns('node',c('SUID','name'),obj=obj)
        node.SUIDs <- dict[which(dict$name  %in% node.names),'SUID']
        return(node.SUIDs)
    }
    
    # initial source used 'which', but it did not return SUIDs in the input names order  
    # dict.indices = which(node.names %in% sapply(obj@node.suid.name.dict, function(n) { n$name}))
    # 'match' achieves this desired behavior
    dict.node.names <- sapply(obj@node.suid.name.dict, function(n) {n$name})
    dict.indices <- match(node.names, dict.node.names)
    
    # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector 
    node.SUIDs <- sapply(obj@node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i) {i$SUID})
    return(node.SUIDs)
}
# ------------------------------------------------------------------------------
.nodeSUIDToNodeName<-function(obj, node.suids) {
    
    if(length(obj@node.suid.name.dict)==0){  # i.e., no obj dictionary
        dict <- getTableColumns('node',c('SUID','name'),obj=obj)
        node.names <- dict[which(dict$SUID  %in% node.suids),'name']
        return(node.names)
    }
    
    dict.node.SUIDs <- sapply(obj@node.suid.name.dict, function(s) {s$SUID})
    dict.indices <- match(node.suids, dict.node.SUIDs)
    
    # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
    node.names <- sapply(obj@node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(n) {n$name})
    return(node.names)
}

# ------------------------------------------------------------------------------
.edgeNameToEdgeSUID<-function(obj, edge.names) {
    
    if(length(obj@edge.node.suid.name.dict)==0){  # i.e., no obj dictionary
        dict <- getTableColumns('edge',c('SUID','name'),obj=obj)
        edge.SUIDs <- dict[which(dict$name  %in% edge.names),'SUID']
        return(edge.SUIDs)
    }
    
    dict.edge.names <- sapply(obj@edge.node.suid.name.dict, function(e) {e$name})
    dict.indices <- match(edge.names, dict.edge.names)
    
    # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
    edge.SUIDs <- sapply(obj@edge.node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i){ i$SUID })
    return(edge.SUIDs)
}

# ------------------------------------------------------------------------------
.edgeSUIDToEdgeName<-function(obj, edge.suids) {
    
    if(length(obj@edge.node.suid.name.dict)==0){  # i.e., no obj dictionary
        dict <- getTableColumns('edge',c('SUID','name'),obj=obj)
        edge.names <- dict[which(dict$SUID  %in% edge.suids),'name']
        return(edge.names)
    }
    
    dict.edge.SUIDs = sapply(obj@edge.node.suid.name.dict, function(s) {s$SUID})
    dict.indices = match(edge.suids, dict.edge.SUIDs)
    
    # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
    edge.names = sapply(obj@edge.node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(e) {e$name})
    return(edge.names)
}


