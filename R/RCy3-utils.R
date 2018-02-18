# ==============================================================================
# Internal utility functions used by more than one RCy3 function. These should
# not be exported, nor visible to package users. Add variable and functions here 
# if you suspect they will be useful for other developers. 
# 
# Dev Note: internal variables and functions should be prefixed with a '.'
# ==============================================================================
# I. Package Variables
# ------------------------------------------------------------------------------
.defaultBaseUrl <- 'http://localhost:1234/v1'

# ==============================================================================
# I. Package Utility Functions
# ------------------------------------------------------------------------------
# Validate and provide user feedback when hex color codes are required input
.isNotHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        write (sprintf ('Error. %s is not a valid hexadecimal color (has to begin with # and be 7 characters long).', color), stderr ())
        return(TRUE)
    }else{
        return(FALSE)
    }
}

# ------------------------------------------------------------------------------
# Replaces node names with SUIDs
.nodeNameToNodeSUID<-function(node.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        node.SUIDs <- dict[which(dict$name  %in% node.names),'SUID']
        return(node.SUIDs)
}
# ------------------------------------------------------------------------------
# Replaces node SUIDs with names
.nodeSUIDToNodeName<-function(node.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        node.names <- dict[which(dict$SUID  %in% node.suids),'name']
        return(node.names)
}

# ------------------------------------------------------------------------------
# Replaces edge names with SUIDs
.edgeNameToEdgeSUID<-function(edge.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.SUIDs <- dict[which(dict$name  %in% edge.names),'SUID']
        return(edge.SUIDs)
}

# ------------------------------------------------------------------------------
# Replaces edge SUIDs with names
.edgeSUIDToEdgeName<-function(edge.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.names <- dict[which(dict$SUID  %in% edge.suids),'name']
        return(edge.names)
}
