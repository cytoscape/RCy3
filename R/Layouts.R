# ==============================================================================
# Functions for performing LAYOUTS in addition to getting and setting layout
# properties. 
# 
# I. Perform layout functions
# II. Get layout properties
# III. Set layout properties
#
# ==============================================================================
# I. Perform layout functions
# ------------------------------------------------------------------------------
#' @title Bundle Edges
#'
#' @description Apply edge bundling to the network specified. Edge bundling is 
#' executed with default parameters; optional parameters are not supported.
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' bundleEdges()
#' }
#' @export
bundleEdges <- function(network = NULL, base.url = .defaultBaseUrl) {
    suid <- getNetworkSuid(network,base.url)
    
    res<-cyrestGET(paste('apply/edgebundling',suid, sep='/'), base.url =  base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @title Apply a layout to a network
#'
#' @details Run \link{getLayoutNames} to list available layouts. 
#' @param layout.name (\code{character}) Name of the layout (with optional parameters). 
#' If not specified, then the preferred layout set in the Cytoscape UI is applied.
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples
#' \donttest{
#' layoutNetwork()
#' layoutNetwork('force-directed')
#' layoutNetwork('force-directed defaultSpringCoefficient=.00006 defaultSpringLength=80')
#' }
#' @export
layoutNetwork <- function(layout.name = NULL, network = NULL, base.url = .defaultBaseUrl) {
    suid <- getNetworkSuid(network,base.url)
    if(is.null(layout.name)){
        res<-commandsPOST(paste0('layout apply preferred networkSelected="SUID:',suid,'"'), base.url=base.url)
        invisible(res)
    } else {
        res <- commandsPOST(paste0('layout ',layout.name,' network="SUID:',suid,'"'),base.url=base.url)
        invisible(res)
    }
}

# ------------------------------------------------------------------------------
#' @title Copy a layout from one network to another
#'
#' @description Sets the coordinates for each node in the target network to the 
#' coordinates of a matching node in the source network.
#' @details Optional parameters such as \code{gridUnmapped} and \code{selectUnmapped} 
#' determine the behavior of target network nodes that could not be matched.
#' @param sourceNetwork (\code{character}) The name of network to get node coordinates from 
#' @param targetNetwork (\code{character}) The name of the network to apply coordinates to
#' @param sourceColumn (optional \code{character}) The name of column in the sourceNetwork node 
#' table used to match nodes; default is 'name'
#' @param targetColumn (optional \code{character}) The name of column in the targetNetwork node 
#' table used to match nodes; default is 'name'
#' @param gridUnmapped (optional \code{character}) If this is set to true, any nodes in the target 
#' network that could not be matched to a node in the source network will be laid out in a grid; 
#' default is TRUE
#' @param selectUnmapped optional \code{character}) If this is set to true, any nodes in the target network 
#' that could not be matched to a node in the source network will be selected in the target network; 
#' default is TRUE
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples
#' \donttest{
#' layoutCopycat('network1','network2')
#' }
#' @export
layoutCopycat <- function(sourceNetwork, targetNetwork, sourceColumn='name', targetColumn='name', 
                          gridUnmapped=TRUE, selectUnmapped=TRUE, base.url=.defaultBaseUrl){
    sourceNetwork <- getNetworkName(sourceNetwork)
    targetNetwork <- getNetworkName(targetNetwork)
    res<-commandsPOST(paste0('layout copycat sourceNetwork="', sourceNetwork,'" targetNetwork="',
                             targetNetwork,'" sourceColumn="',sourceColumn,'" targetColumn="',
                             targetColumn,'" gridUnmapped="',gridUnmapped,'" selectUnmapped="',
                             selectUnmapped), base.url=base.url)
    invisible(res)
}

# ==============================================================================
# II. Get layout properties
# ------------------------------------------------------------------------------
#' @title Get Layout Names
#' 
#' @description Retrieve the names of the currently supported layout algorithms.  These
#' may be used in subsequent calls to the 'layoutNetwork' function.  
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of \code{character} strings, e.g., "force-directed" "circular" "grid"
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutNames()
#' # [1] "degree-circle"         "attributes-layout"      "kamada-kawai"                    
#' # [4] "force-directed"        "cose"                   "hierarchical"                    
#' # [7] "attribute-circle"      "stacked-node-layout"    "circular"
#' }
#' @export
getLayoutNames <- function(base.url=.defaultBaseUrl) {
    res <- cyrestGET("apply/layouts",base.url=base.url)
    available.layouts <- unname(res)
    return(available.layouts)
}

# ------------------------------------------------------------------------------
#' @title Get Layout Name Mapping
#' 
#' @description The Cytoscape 'Layout' menu lists many layout algorithms, but the names presented 
#' there are different from the names by which these algorithms are known to layout method. This 
#' method returns a named list in which the names are from the GUI, and the values identify the 
#' names you must use to choose an algorithms in the programmatic interface.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A named \code{list} of \code{character} strings
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutNameMapping()
#' # Degree Sorted Circle Layout    Group Attributes Layout    Edge-weighted Spring Embedded Layout 
#' #              "degree-circle"       "attributes-layout"                          "kamada-kawai" 
#' }
#' @export
getLayoutNameMapping <- function(base.url=.defaultBaseUrl) {
    layout.names <- getLayoutNames(base.url)
    layout.full.names <- c()
    
    # get the full name of a layout
    for (layout.name in layout.names){
        res <- cyrestGET(paste("apply/layouts", as.character(layout.name), sep="/"),base.url=base.url)
        layout.property.names <- unname(res)
        layout.full.names <- c(layout.full.names, layout.property.names[[4]])
    }
    names(layout.names) <- layout.full.names
    return(layout.names)
}

# ------------------------------------------------------------------------------
#' @title Get Layout Property Names
#' 
#' @description Returns a list of the tunable properties for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts.
#' @param layout.name (\code{character}) Name of the layout
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list} of \code{character} strings
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyNames('force-directed')
#' # [1] "numIterations"            "defaultSpringCoefficient" "defaultSpringLength"         
#' # [4] "defaultNodeMass"          "isDeterministic"          "singlePartition"     
#' }
#' @export
getLayoutPropertyNames <- function(layout.name, base.url = .defaultBaseUrl) {
    res <- cyrestGET(paste("apply/layouts", as.character(layout.name), "parameters/", sep="/"),base.url=base.url)
    layout.property.names <- unname(res)
    return(vapply(layout.property.names, '[[', character(1), 1))
}

# ------------------------------------------------------------------------------
#' @title Get Layout Property Type
#' 
#' @description Returns the type of one of the tunable properties (property.name) for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param property.name (\code{character}) Name of the property
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{character} string specifying the type
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyType('force-directed','defaultSpringLength')
#' # "double"
#' }
#' @export
getLayoutPropertyType <- function(layout.name, property.name, base.url = .defaultBaseUrl) {
    res <- cyrestGET(paste("apply/layouts", as.character(layout.name), "parameters/", sep="/"),base.url=base.url)
    layout.property.list <- unname(res)
    layout.property.names <- vapply(layout.property.list, '[[', character(1), 1)
    position <- layout.property.names == property.name
    return(vapply(layout.property.list, '[[', character(1), 3)[position])
}

#------------------------------------------------------------------------------------------------------------------------
#' @title Get Layout Property Value
#' 
#' @description Returns the appropriately typed value of the specified tunable property for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param property.name (\code{character}) Name of the property
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return The current value set for this layout property. Typically an \code{integer}, \code{numeric} or \code{character} string value.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyValue('force-directed','defaultSpringLength')
#' # 80
#' }
#' @export
getLayoutPropertyValue <- function (layout.name, property.name, base.url = .defaultBaseUrl) {
    res <- cyrestGET(paste("apply/layouts", as.character(layout.name), "parameters", sep="/"),base.url = base.url)
    layout.property.list <- unname(res)
    layout.property.names <- vapply(layout.property.list, '[[', character(1), 1)
    position <- layout.property.names == property.name
    return(sapply(layout.property.list, '[[', 4)[position]) #can't use vapply: various types of values returned
}

# ==============================================================================
# III. Set layout properties
#------------------------------------------------------------------------------------------------------------------------
#' @title Set Layout Properties
#' 
#' @description Sets the specified properties for the specified layout. Unmentioned properties are left unchanged.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param properties.list (\code{list}) List of one or more \code{property=value} pairs
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' setLayoutProperties('force-directed', list(defaultSpringLength=50, defaultSpringCoefficient=6E-04))
#' # Successfully updated the property 'defaultSpringLength'.
#' # Successfully updated the property 'defaultSpringCoefficient'.
#' }
#' @export
setLayoutProperties <- function (layout.name, properties.list, base.url = .defaultBaseUrl) {
    all.possible.props <- getLayoutPropertyNames (layout.name, base.url)
    
    # collect properties and values into nested named list
    all.properties = list()
    for (prop in names (properties.list)) {
        if (!prop %in% all.possible.props) {
            write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
        } else {
            new.value <- properties.list [[prop]]
            each.property <- list("name"=prop, "value"=new.value)
            res <- cyrestPUT(paste("apply/layouts", as.character(layout.name), "parameters", sep="/"),
                             body=list(each.property), 
                             base.url=base.url)
        }
    }
}

