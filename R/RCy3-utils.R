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
# Validate and provide user feedback when hex color codes are required input.
.isNotHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        write (sprintf ('Error. %s is not a valid hexadecimal color (has to begin with # and be 7 characters long).', color), stderr ())
        return(TRUE)
    }else{
        return(FALSE)
    }
}

# ------------------------------------------------------------------------------
# Replaces node names with SUIDs.
.nodeNameToNodeSUID<-function(node.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        test <- vapply(node.names, function(x){x %in% dict[,'SUID']}, logical(1))
        if(all(test))  #provided SUIDs already!
            return(node.names)
        node.SUIDs <- dict$SUID[match(node.names, dict$name)]
        return(node.SUIDs)
}
# ------------------------------------------------------------------------------
# Replaces node SUIDs with names.
.nodeSUIDToNodeName<-function(node.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        test <- vapply(node.suids, function(x){x %in% dict[,'name']}, logical(1))
        if(all(test)) #provided names already!
            return(node.suids)
        node.names <- dict$name[match(node.suids, dict$SUID)]
        return(node.names)
}

# ------------------------------------------------------------------------------
# Replaces edge names with SUIDs.
.edgeNameToEdgeSUID<-function(edge.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.SUIDs <- dict$SUID[match(edge.names, dict$name)]
        return(edge.SUIDs)
}

# ------------------------------------------------------------------------------
# Replaces edge SUIDs with names.
.edgeSUIDToEdgeName<-function(edge.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.names <- dict$name[match(edge.suids, dict$SUID)]
        return(edge.names)
}

# ------------------------------------------------------------------------------
# Checks to see if a particular column name exists in the specific table. Returns
# TRUE or FALSE.
.tableColumnExists <- function(table.column, table, network=network, base.url=base.url){
    if (!table.column %in% getTableColumnNames(table, network=network, base.url=base.url)) {
        write (sprintf ('Column %s does not exist in the %s table.', table.column, table), stderr ())
        return (FALSE)
    }
    return (TRUE)
}

# ------------------------------------------------------------------------------
# Checks to see if min supported versions of api and cytoscape are running.
# Extracts numerics from api and major cytoscape versions before making comparison.
.verifySupportedVersions<-function(cyrest=1,cytoscape=3.6,base.url=.defaultBaseUrl) {
    vStr <- cytoscapeVersionInfo(base.url)
    vApiStr <- unname(vStr[1])
    vCyStr <- unname(vStr[2])
    vApiNum <- as.numeric(gsub("v([0-9]+)$", "\\1", vApiStr))
    vCyNum <- as.numeric(gsub("([0-9]+\\.[0-9]+)\\..*$", "\\1", vCyStr))
    
    nogo <- FALSE
    if(cyrest > vApiNum){
        write(sprintf("CyREST API version %d or greater is required. You are currently working with version %d.",
                      cyrest, vApiNum),
              stderr())
        nogo <- TRUE
    }
    if(cytoscape > vCyNum){
        write(sprintf("Cytoscape version %0.2g or greater is required. You are currently working with version %0.2g.",
                      cytoscape, vCyNum),
              stderr())
        nogo <- TRUE
    }
    if(nogo)
        stop(simpleError("Function not run due to unsupported version."))
}