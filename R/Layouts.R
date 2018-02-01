# TODO: layoutEdgeBundling

# ------------------------------------------------------------------------------
#' Get Layout Names
#' 
#' @description Retrieve the names of the currently supported layout algorithms.  These
#' may be used in subsequent calls to the 'layoutNetwork' function.  
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return A \code{list} of \code{character} strings, e.g., "force-directed" "circular" "grid"
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutNames()
#' # [1] "degree-circle"                    "attributes-layout"                "kamada-kawai"                    
#' # [4] "force-directed"                   "cose"                             "hierarchical"                    
#' # [7] "attribute-circle"                 "stacked-node-layout"              "circular"
#' }
#' @export
#' @title getLayoutNames
getLayoutNames <- function(base.url=.defaultBaseUrl) {
    request.uri <- paste(base.url, "apply/layouts", sep="/")
    request.res <- GET(url=request.uri)
    
    available.layouts <- unname(fromJSON(rawToChar(request.res$content)))
    return(available.layouts)
}

# ------------------------------------------------------------------------------
#' Get Layout Name Mapping
#' 
#' @description The Cytoscape 'Layout' menu lists many layout algorithms, but the names presented 
#' there are different from the names by which these algorithms are known to layout method. This 
#' method returns a named list in which the names are from the GUI, and the values identify the 
#' names you must use to choose an algorithms in the programmatic interface.
#' @param base.url (optional)  URL prefix for CyREST calls
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
    
    # get the English/full name of a layout
    for (layout.name in layout.names){
        request.uri <- paste(base.url, "apply/layouts", as.character(layout.name), sep="/")
        request.res <- GET(url=request.uri)
        
        layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
        layout.full.names <- c(layout.full.names, layout.property.names[[4]])
    }
    names(layout.names) <- layout.full.names
    
    return(layout.names)
}

# ------------------------------------------------------------------------------
#' Get Layout Property Names
#' 
#' @description Returns a list of the tunable properties for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts.
#' @param layout.name (\code{character}) Name of the layout
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return A \code{list} of \code{character} strings
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyNames('force-directed')
#' # [1] "numIterations"            "defaultSpringCoefficient" "defaultSpringLength"         
#' # [4] "defaultNodeMass"          "isDeterministic"          "singlePartition"     
#' }
#' @export
getLayoutPropertyNames <- function(layout.name, base.url = .defaultBaseUrl) {
    request.uri <- paste(base.url, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
    request.res <- GET(url=request.uri)
    
    layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
    return(sapply(layout.property.names, '[[', 1))
}

# ------------------------------------------------------------------------------
#' Get Layout Property Type
#' 
#' @description Returns the type of one of the tunable properties (property.name) for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param property.name (\code{character}) Name of the property
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return A \code{character} string specifying the type
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyType('force-directed','defaultSpringLength')
#' # "double"
#' }
#' @export
getLayoutPropertyType <- function(layout.name, property.name, base.url = .defaultBaseUrl) {
    request.uri <- paste(base.url, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
    request.res <- GET(url=request.uri)
    
    layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
    layout.property.names <- sapply(layout.property.list, '[[', 1)
    position <- layout.property.names == property.name
    return(sapply(layout.property.list, '[[', 3)[position])
}

#------------------------------------------------------------------------------------------------------------------------
#' Get Layout Property Value
#' 
#' @description Returns the appropriately typed value of the specified tunable property for the specified layout.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param property.name (\code{character}) Name of the property
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return The current value set for this layout property. Typically an \code{integer}, \code{numeric} or \code{character} string value.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' getLayoutPropertyValue('force-directed','defaultSpringLength')
#' # 80
#' }
#' @export
getLayoutPropertyValue <- function (layout.name, property.name, base.url = .defaultBaseUrl) {
    request.uri <- paste(base.url, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
    request.res <- GET(url=request.uri)
    
    layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
    layout.property.names <- sapply(layout.property.list, '[[', 1)
    position <- layout.property.names == property.name
    return(sapply(layout.property.list, '[[', 4)[position])
}

#------------------------------------------------------------------------------------------------------------------------
#' Set Layout Properties
#' 
#' @description Sets the specified properties for the specified layout. Unmentioned properties are left unchanged.
#' @details Run \link{getLayoutNames} to list available layouts. Run \link{getLayoutPropertyNames} to list properties per layout.
#' @param layout.name (\code{character}) Name of the layout
#' @param property.list (\code{list}) List of one or more \code{property=value} pairs
#' @param base.url (optional)  URL prefix for CyREST calls
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
    
    # set properties iteratively, this could have been done with a single API call
    for (prop in names (properties.list)) {
        if (!prop %in% all.possible.props) {
            write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
        } else {
            new.value <- properties.list [[prop]]
            new.property.value.list <- list("name"=prop, "value"=new.value)
            new.property.value.list.JSON <- toJSON(list(new.property.value.list))
            
            request.uri <- paste(base.url, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
            request.res <- PUT(url=request.uri, body= new.property.value.list.JSON, encode="json")
            if (request.res$status == 200){
                write (sprintf ("Successfully updated the property '%s'.", prop), stdout ())
            } else {
                write (sprintf ("Something went wrong. Unable to update property '%s'.", prop), stderr ())
            }
            invisible(request.res)
        }
    } # for prop
}

# ------------------------------------------------------------------------------
#' Apply a layout to a network
#'
#' @param layout (char) Name of the layut, run commandHelp('layout') to see available list of layouts.
#' Feel free to include parameters along with the layout name, space delimited. See example.
#' @details Run \link{getLayoutNames} to list available layouts. 
#' @param layout.name (\code{character}) Name of the layout. If not specified, then the preferred layout set in the Cytoscape UI is applied.
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return None
#' @examples
#' \donttest{
#' layoutNetwork('force-directed defaultSpringCoefficient=.00006 defaultSpringLength=80')
#' }
#' @export
layoutNetwork <- function(layout.name = NULL, network = NULL, base.url = .defaultBaseUrl) {
    if(is.null(network)){
        suid = getNetworkSuid()
    }else if(class(network)=='character'){
        suid = getNetworkSuid(title=network)
    } else if(class(network)=='numeric'){
        suid = network
    } else {
        stop("ERROR: Invalid network. Provide network name, suid or NULL for current network.")
    }
    
    if(is.null(layout.name)){
        res<-commandRun(paste0('layout apply preferred networkSelected="SUID:"',suid), base.url)
        invisible(res)
    } else if(!layout.name %in% getLayoutNames(base.url)) {
        write(sprintf("layout.name '%s' is not recognized; call getLayoutNames(<CytoscapeWindow>) to see those which are supported", layout.name), stderr())
    } else {
        api.str <- paste(base.url, "apply/layouts", layout.name, suid, sep = "/")
        res <- GET(api.str)
        invisible(res)
    }
}

# ------------------------------------------------------------------------------
#' Copy a layout from one network to another
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
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return None
#' @examples
#' \donttest{
#' layoutCopycat('network1','network2')
#' }
#' @export
layoutCopycat <- function(sourceNetwork, targetNetwork, sourceColumn='name', targetColumn='name', 
              gridUnmapped=TRUE, selectUnmapped=TRUE, base.url=.defaultBaseUrl){
    res<-commandRun(paste0('layout copycat sourceNetwork="', sourceNetwork,'" targetNetwork="',
                           targetNetwork,'" sourceColumn="',sourceColumn,'" targetColumn="',
                           targetColumn,'" gridUnmapped="',gridUnmapped,'" selectUnmapped="',
                           selectUnmapped), base.url)
    invisible(res)
}