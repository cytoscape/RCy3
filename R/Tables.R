# ------------------------------------------------------------------------------
#' @title Get table column values
#'
#' @description Retrieve one or more columns of data from node, edge or network
#' tables.
#' @details The 'suid' column is always retrieved along with specified columns. 
#' The 'suid' values are used as \code{row.names} in the returned \code{data.frame}. 
#' @param table name of table, e.g., node (default), edge, network
#' @param columns names of columns to retrieve values from as list object or comma-separated list; default is all columns
#' @param namespace namespace of table; default is "default"
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return A \code{data.frame} of column values
#' @examples
#' \donttest{
#' getTableColumns()
#' getTableColumns('node','group')
#' }
#' @export
getTableColumns <- function(table = 'node',
                             columns = NULL,
                             namespace = 'default',
                             network = NULL,
                             base.url = .defaultBaseUrl) {
    #TODO handle column types like getNodeAttributes?
    suid = getNetworkSuid(network)
    
    #all columns
    if (is.null(columns))
        columns = getTableColumnNames(table, namespace, network, base.url)
    
    #handle comma separated lists and list objects
    col.list = columns
    if (length(col.list) == 1)
        col.list = unlist(strsplit(columns, ","))
    
    #get suid column first
    tbl = paste0(namespace, table)
    res.names <- cyrestGET(paste('networks',suid,'tables',tbl,'columns','SUID',sep="/"),
                           base.url = base.url)
    df = data.frame(row.names = res.names$values)
    
    #retrieve column names
    table.col.list = getTableColumnNames(table, namespace, network, base.url)
    
    #then append other requested columns
    for (col in col.list) {
        #check for column names
        if (!col %in% table.col.list) {
            cat(sprintf("Error: Column %s not found in %s table \n", col, table))
            next()
        }
        res.col <- cyrestGET(paste('networks',suid,'tables',tbl,'columns',col, sep="/"),
                             base.url = base.url)
        #convert NULL to NA, then unlist
        cvv <- unlist(lapply(res.col$values, function(x)
            ifelse(is.null(x), NA, x)))
        if (length(res.names$values) == length(cvv)) {
            for (i in 1:length(res.names$values)) {
                df[i, col] <- cvv[i]
            }
        } else {
            print(
                "Warning: Requested column has missing values. Returning single column without row.names..."
            )
            df2 = data.frame(col = unlist(res.col$values))
            return(df2)
        }
    }
    return(df)
}

# ------------------------------------------------------------------------------
#' @title Get Table Column Names 
#'
#' @description Retrieve the names of all columns in a table
#' @param table name of table, e.g., node, edge, network; default is "node"
#' @param namespace namespace of table, e.g., default
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return list of column names
#' @examples
#' \donttest{
#' getTableColumnNames()
#' getTableColumnNames('edge')
#' getTableColumnNames('network')
#' }
#' @export
getTableColumnNames <-  function(table = 'node',
                              namespace = 'default',
                              network = NULL,
                              base.url = .defaultBaseUrl) {
    title = getNetworkName(network)
    cmd = paste(table,
                ' list attributes network="',
                title,
                '" namespace="',
                namespace,
                sep = '')
    return(commandsPOST(cmd, base.url = base.url))
}

# ------------------------------------------------------------------------------
#' @title Loads data into Cytoscape tables keyed by row
#'
#' @description This function loads data into Cytoscape node/edge/network tables provided a
#' common key, e.g., name. Data.frame column names will be used to set Cytoscape table column
#' names.
#' @details Numeric (or integer) values will be stored as Doubles in Cytoscape tables.
#' Character or mixed values will be stored as Strings. Logical values will be stored as Boolean.
#' Existing columns with the same names will be overwritten.
#' @param data (data.frame) each row is a node and columns contain node attributes
#' @param data.key.column (char) name of data.frame column to use as key; default is "row.names"
#' @param table (char) name of Cytoscape table to load data into, e.g., node, edge or network; default is "node"
#' @param table.key.column (char) name of Cytoscape table column to use as key; default is "name"
#' @param namespace namespace of table, e.g., default
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @importFrom BiocGenerics colnames
#' @export
loadTableData <- function(data,
                          data.key.column = 'row.names',
                          table = 'node',
                          table.key.column = 'name',
                          namespace = 'default',
                          network = NULL,
                          base.url = .defaultBaseUrl) {
    
    net.suid <- getNetworkSuid(network)
    table.key.column.values =  getTableColumns(table = table,
                                               columns = table.key.column,
                                               network = net.suid,
                                               base.url = base.url)
    
    ##ERROR if table.key.column.values is empty
    if (ncol(table.key.column.values) == 0)
        return("Failed to load data: Please check table.key.column")
    
    if (data.key.column == 'row.names')
        # if key in row.names...
        data$row.names <-
        row.names(data)  # then copy to new "row.names" column :)
    
    ##ERROR if data.key.column not in data
    if (!data.key.column %in% colnames(data))
        return("Failed to load data: Please check data.key.column")
    
    filter = data[, data.key.column] %in% table.key.column.values[, 1]
    
    ##ERROR if filter is empty
    if (!TRUE %in% filter)
        return("Failed to load data: Provided key columns do not contain any matches")
    
    data.subset = data[filter, ]
    
    #remove troublesome factors
    if (class(data.subset[, data.key.column]) == "factor")
        data.subset[, data.key.column] = levels(droplevels(data.subset[, data.key.column]))
    
    data.list <- c()
    for (i in 1:dim(data.subset)[1]) {
        #each rows
        rest <- list()
        for (j in 1:dim(data.subset)[2]) {
            #each column
            rest[[colnames(data.subset)[j]]] = data.subset[i, j]
        }
        data.list[[i]] <- rest
    }
    
    table = paste0(namespace, table) #add prefix
    
    tojson.list <-
        list(key = table.key.column,
             dataKey = data.key.column,
             data = data.list)
    
    cyrestPUT(paste('networks', net.suid, "tables", table, sep = '/'),
              body = tojson.list,
              base.url = base.url)
    return(sprintf("Success: Data loaded in %s table", table))
}

# ------------------------------------------------------------------------------
#' @title Delete a table column 
#'
#' @description Delete a column from node, edge or network tables.
#' @param columns Name of the column to delete
#' @param table Name of table, e.g., node (default), edge, network
#' @param namespace Namespace of table. Default is "default".
#' @param network Name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return A \code{data.frame} of column values
#' @examples
#' \donttest{
#' deleteTableColumn('node','group')
#' }
#' @export
deleteTableColumn <- function(column ,
                               table = 'node',
                               namespace = 'default',
                                network = NULL, 
                                base.url = .defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    tbl = paste0(namespace, table)
    cyrestDELETE(paste('networks',net.suid,'tables',tbl,'columns',as.character(column),sep="/"),
                           base.url = base.url)
}


