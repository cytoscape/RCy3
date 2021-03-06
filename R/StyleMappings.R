# ==============================================================================
# Functions for defining MAPPINGS between table column values and visual properties,
# organized into sections:
#
# I. General functions for creating and applying mappings for node, edge and network properties
# II. Specific functions for defining particular node, edge and network properties
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' Creates a mapping between an attribute and a visual property
#'
#' @description Generates the appropriate data structure for the "mapping" parameter
#' in updateStyleMapping.
#' @details The paired list of values must be of the same length or mapping will fail.
#' For gradient mapping, you may include two additional visual.prop.values in the 
#' first and last positions to map respectively to values less than and greater 
#' than those specified in table.column.values. Mapping will also fail if the 
#' data type of table.column.values does not match that of the existing table.column. 
#' Note that all imported numeric data are stored as Integers or Doubles in 
#' Cytosacpe tables; and character or mixed data are stored as Strings.
#' @param visual.prop (char) name of visual property to map
#' @param table.column (char) name of table column to map
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p)
#' @param table.column.values (list) list of values paired with visual.prop.values; skip for passthrough mapping
#' @param visual.prop.values (list) list of values paired with table.column.values; skip for passthrough mapping
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return (network=network, base.url=base.url) ready to convert into JSON by style mapping operations
#' @seealso updateStyleMapping getVisualPropertyNames
#' @examples
#' \donttest{
#' mapVisualProperty('node fill color','score','c',c(-4.0,0.0,9.0),c('#99CCFF','#FFFFFF','#FF7777'))
#' mapVisualProperty('node shape','type','d',c('protein','metabolite'),c('ellipse','rectangle'))
#' mapVisualProperty('node label','alias','p')
#' }
#' @section List of visual properties:
#' \tabular{lll}{
#' Node Border Stroke \tab Edge Bend \tab Network Background Paint \cr
#' Node Border Paint \tab Edge Curved \tab Network Center X Location \cr
#' Node Border Transparency \tab Edge Label \tab Network Center Y Location \cr
#' Node Border Width \tab Edge Label Color \tab Network Center Z Location \cr
#' Node CustomGraphics 1-9 \tab Edge Label Font Face \tab Network Depth \cr
#' Node CustomGraphics Position 1-9 \tab Edge Label Font Size \tab Network Edge Selection \cr
#' Node CustomGraphics Size 1-9 \tab Edge Label Transparency \tab Network Height \cr
#' Node CustomPaint 1-9 \tab Edge Label Width \tab Network Node Selection \cr
#' Node Depth \tab Edge Line Type \tab Network Scale Factor \cr
#' Node Fill Color \tab Edge Paint \tab Network Size \cr
#' Node Height \tab Edge Selected \tab Network Title \cr
#' Node Label \tab Edge Selected Paint \tab Network Width \cr
#' Node Label Color \tab Edge Source Arrow Selected Paint \tab  \cr
#' Node Label Font Face \tab Edge Source Arrow Shape \tab  \cr
#' Node Label Font Size \tab Edge Source Arrow Size \tab  \cr
#' Node Label Position \tab Edge Source Arrow Unselected Paint \tab  \cr
#' Node Label Transparency \tab Edge Stroke Selected Paint \tab  \cr
#' Node Label Width \tab Edge Stroke Unselected Paint \tab  \cr
#' Node Network Image Visible \tab Edge Target Arrow Selected Paint \tab  \cr
#' Node Paint \tab Edge Target Arrow Shape \tab  \cr
#' Node Selected \tab Edge Target Arrow Size \tab  \cr
#' Node Selected Paint \tab Edge Target Arrow Unselected Paint \tab  \cr
#' Node Shape \tab Edge Tooltip \tab  \cr
#' Node Size \tab Edge Transparency \tab  \cr
#' Node Tooltip \tab Edge Unselected Paint \tab  \cr
#' Node Transparency \tab Edge Visible \tab  \cr
#' Node Visible \tab Edge Visual Property \tab  \cr
#' Node Width \tab Edge Width \tab  \cr
#' Node X Location \tab  \tab  \cr
#' Node Y Location \tab  \tab  \cr
#' Node Z Location \tab  \tab  \cr
#' }
#' @export
mapVisualProperty <- function(visual.prop, table.column, mapping.type, table.column.values,
                              visual.prop.values, network=NULL, base.url=.defaultBaseUrl){
    
    suid <- getNetworkSuid(network,base.url)
    
    #process mapping type
    mapping.type.name = switch(mapping.type, 'c'='continuous','d'='discrete','p'='passthrough',mapping.type)
    
    #processs visual property, including common alternatives for vp names :)
    visual.prop.name = toupper(gsub("\\s+","_",visual.prop))
    visual.prop.name = switch(visual.prop.name,
                              'EDGE_COLOR'='EDGE_UNSELECTED_PAINT',
                              'EDGE_THICKNESS'='EDGE_WIDTH',
                              'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                              'NODE_BORDER_LINE_TYPE'='NODE_BORDER_STROKE',
                              visual.prop.name)
    
    #check visual prop name
    if(!visual.prop.name %in% getVisualPropertyNames(base.url = base.url)){
        stop(paste0('Could not find ',visual.prop.name,
                     '. Run getVisualPropertyNames() to retrieve property names.'))
    }
    
    #check mapping column and get type
    tp = tolower(strsplit(visual.prop.name,"_")[[1]][1])
    table = paste0('default',tp)
    res <- cyrestGET(paste('networks',suid,'tables',table,'columns',sep='/'),base.url=base.url)
    table.column.type = NULL
    for(i in seq_len(length(res))){
        if(res[[i]]$name==table.column){
            table.column.type = res[[i]]$type
            break
        }
    }
    if(is.null(table.column.type))
        stop(paste0('Could not find ',table.column,' column in ',table,' table.'))
    
    #construct visual property map
    visual.prop.map <- list(
        mappingType=mapping.type.name,
        mappingColumn=table.column,
        mappingColumnType=table.column.type,
        visualProperty=visual.prop.name
    )
    
    if(mapping.type.name=='discrete'){
        map <- list()
        for (i in seq_len(length(table.column.values))) {
            map[[i]] <- list(key=toString(table.column.values[i]), value=toString(visual.prop.values[i]))
        }
        visual.prop.map$map=map
    }else if(mapping.type.name=='continuous'){
        points <- list()
        #check for extra lesser and greater values
        propValCount <- length(visual.prop.values)
        colValCount <- length(table.column.values)
        if (propValCount-colValCount == 2){
            for (i in seq_len(colValCount)) {
                points[[i]] <- list(value=table.column.values[i],
                                    lesser=visual.prop.values[i+1],
                                    equal=visual.prop.values[i+1], 
                                    greater=visual.prop.values[i+1]) #note offset
            }
            #then correct extreme values
            points[[1]] <- list(value=table.column.values[1],
                                lesser=visual.prop.values[1],
                                equal=visual.prop.values[2], 
                                greater=visual.prop.values[2]) 
            points[[colValCount]] <- list(value=table.column.values[colValCount],
                                          lesser=visual.prop.values[colValCount+1],
                                          equal=visual.prop.values[colValCount+1], 
                                          greater=visual.prop.values[colValCount+2]) 
        }
        else if (propValCount-colValCount == 0) {
            for (i in seq_len(colValCount)) {
                points[[i]] <- list(value=table.column.values[i],
                                    lesser=visual.prop.values[i],
                                    equal=visual.prop.values[i],
                                    greater=visual.prop.values[i])
            }
        }
        else {
            stop ("Table.column.values and visual.prop.values don't match up.")
        }
        visual.prop.map$points=points
    }
    return(visual.prop.map)
}

# ------------------------------------------------------------------------------
#' Updates a visual property mapping in a style
#'
#' @description Updates the visual property mapping, overriding any prior mapping. Creates a
#' visual property mapping if it doesn't already exist in the style.
#' @details Requires visual property mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param mapping a single visual property mapping, see mapVisualProperty
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @export
#' @seealso mapVisualProperty
#' @examples
#' \donttest{
#' updateStyleMapping('myStyle',mapVisualProperty('node label','name','p'))
#' }
updateStyleMapping <- function(style.name, mapping, base.url=.defaultBaseUrl){
    visual.prop.name = mapping$visualProperty
    # set default style
    if(is.null(style.name)){
        style.name <- 'default'
        message('style.name not specified, so updating "default" style.')
    }
    # check if vp exists already
    res <- cyrestGET(paste('styles', style.name,'mappings',sep = '/')
                     ,base.url=base.url)
    vp.list <- lapply(res, function(x) unname(x['visualProperty'][[1]]))
    if(visual.prop.name %in% vp.list){
        exists = TRUE
    } else {
        exists = FALSE
    }
    
    if(exists==TRUE){     
        res <- invisible(cyrestPUT(paste('styles', style.name,'mappings',visual.prop.name, sep = '/'),
                            body=list(mapping), 
                            base.url=base.url))
    }
    else {   
        res <- invisible(cyrestPOST(paste('styles', style.name,'mappings',sep = '/'),
                             body=list(mapping), 
                             base.url=base.url))
    }
    Sys.sleep(get(".MODEL_PROPAGATION_SECS", envir=RCy3env)) ## NOTE: TEMPORARY SLEEP "FIX" 
    return(res)
}

# ------------------------------------------------------------------------------
#' @title Delete Style Mapping
#'
#' @description Deletes a specified visual style mapping from specified style.
#' @param style.name (char) name for style
#' @param visual.prop (char) name of visual property to map. 
#' See getVisualPropertyNames().
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' deleteStyleMapping()
#' }
#' @export
deleteStyleMapping<-function(style.name, visual.prop,  base.url=.defaultBaseUrl){
    # check if vp exists already
    res <- cyrestGET(paste('styles', style.name,'mappings',sep = '/')
                     ,base.url=base.url)
    vp.list <- lapply(res, function(x) unname(x['visualProperty'][[1]]))
    if(visual.prop %in% vp.list){
        exists = TRUE
    } else {
        exists = FALSE
    }
    
    if(exists==TRUE){     
        invisible(cyrestDELETE(paste('styles', style.name,'mappings',visual.prop, sep = '/'),
                            base.url=base.url))
    }
}

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. Node Properties
# Pattern: (1) prepare mapVisualProperty, (2) call updateStyleMapping()
# ------------------------------------------------------------------------------
#' @title Set Node Border Color Mapping
#'
#' @description Map table column values to colors to set the node border color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setNodeBorderColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                       mapping.type='c', default.color=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default color
    if(!is.null(default.color))
        setNodeBorderColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_BORDER_PAINT",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_BORDER_PAINT",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_BORDER_PAINT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 

# ------------------------------------------------------------------------------
#' @title Set Node Border Opacity Mapping
#'
#' @description Sets opacity for node border only.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped 
#' values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderOpacityMapping('score', c(-5,5), c(50,255))
#' }
#' @export
setNodeBorderOpacityMapping <- function(table.column, table.column.values=NULL, 
                                        opacities=NULL, mapping.type='c', 
                                        default.opacity=NULL, style.name=NULL, 
                                        network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    for (o in opacities){
        .checkOpacity(o)
    }
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity)
        setVisualPropertyDefault(
            list(visualProperty = "NODE_BORDER_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp1 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("Mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Node Border Width Mapping
#'
#' @description Map table column values to widths to set the node border width.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param widths List of width values to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.width Width value to set as default for all unmapped values 
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderWidthMapping('score', c(0,30), c(1,5))
#' }
#' @export
setNodeBorderWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                       mapping.type='c', default.width=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.width))
        setNodeBorderWidthDefault(default.width, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_BORDER_WIDTH",table.column, 'c',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_BORDER_WIDTH",table.column, 'd',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_BORDER_WIDTH",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Node Color Mapping
#'
#' @description Map table column values to colors to set the node fill color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors List of hex colors to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeColorMapping('score', c(-5,0,5), c('#5577FF','#FFFFFF','#FF7755'))
#' }
#' @export
setNodeColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                 mapping.type='c', default.color=NULL, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setNodeColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_FILL_COLOR",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_FILL_COLOR",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_FILL_COLOR",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 

# ------------------------------------------------------------------------------
#' @title Set Node Combo Opacity Mapping
#'
#' @description Sets opacity for node fill, border and label all together.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped 
#' values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeComboOpacityMapping('score', c(-5,5), c(50,255))
#' }
#' @export
setNodeComboOpacityMapping <- function (table.column, table.column.values=NULL, 
                                        opacities=NULL, mapping.type='c',
                                        default.opacity=NULL, style.name=NULL, 
                                        network=NULL, base.url=.defaultBaseUrl) {
    
    # check if opacities are formatted correctly
    for (o in opacities){
        .checkOpacity(o)
    }
    
    # set default
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity)
        setVisualPropertyDefault(
            list(visualProperty = "NODE_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
        setVisualPropertyDefault(
            list(visualProperty = "NODE_BORDER_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
        setVisualPropertyDefault(
            list(visualProperty = "NODE_LABEL_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
        mvp2 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
        mvp3 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
        mvp2 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
        mvp3 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
        mvp2 <- mapVisualProperty("NODE_BORDER_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
        mvp3 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
    updateStyleMapping(style.name, mvp2, base.url = base.url)
    updateStyleMapping(style.name, mvp3, base.url = base.url)
} 

# ------------------------------------------------------------------------------
#' @title Set Node Fill Opacity Mapping
#'
#' @description Sets opacity for node fill only.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped 
#' values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeFillOpacityMapping('score', c(-5,5), c(50,255))
#' }
#' @export
setNodeFillOpacityMapping <- function(table.column, table.column.values=NULL, 
                                        opacities=NULL, mapping.type='c', 
                                        default.opacity=NULL, style.name=NULL, 
                                        network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    for (o in opacities){
        .checkOpacity(o)
    }
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity) 
        setVisualPropertyDefault(
            list(visualProperty = "NODE_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("NODE_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Font Face Mapping
#'
#' @description Sets font face for node labels.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param fonts List of string specifications of font face, style and size, e.g., 
#' c("SansSerif,plain,12", "Dialog,plain,10")
#' @param mapping.type (char) discrete or passthrough (d,p); 
#' default is discrete
#' @param default.font String specification of font face, style and size, e.g., 
#' "SansSerif,plain,12" or "Dialog,plain,10"
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeFontFaceMapping("myfonts", c("normal","small"),
#' c("SansSerif,plain,12", "Dialog,plain,10"))
#' }
#' @export
setNodeFontFaceMapping <- function(table.column, table.column.values, 
                                   fonts, mapping.type='d', 
                                   default.font=NULL,
                                   style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    if(!is.null(default.font)){
        setVisualPropertyDefault(
            list(visualProperty = "NODE_LABEL_FONT_FACE", value = default.font), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        stop("Continuous mapping of font face values is not supported.")
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("NODE_LABEL_FONT_FACE",table.column, 'd',
                                  table.column.values, fonts, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("NODE_LABEL_FONT_FACE",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Mapping
#'
#' @description Map table column values to sizes to set the node size.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param sizes List of size values to mape to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.size Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeFontSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeFontSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                mapping.type='c', default.size=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.size))
        setNodeFontSizeDefault(default.size, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_LABEL_FONT_SIZE",table.column, 'c',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_LABEL_FONT_SIZE",table.column, 'd',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_LABEL_FONT_SIZE",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Height Mapping
#'
#' @description Map table column values to the node heights.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param heights List of height values to mape to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.height Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeHeightMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeHeightMapping <- function (table.column, table.column.values=NULL, heights=NULL, 
                                mapping.type='c', default.height=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.height))
        setNodeHeightDefault(default.height, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_HEIGHT",table.column, 'c',
                                 table.column.values, heights, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_HEIGHT",table.column, 'd',
                                 table.column.values, heights, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_HEIGHT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Label Mapping
#'
#' @description Pass the values from a table column to display as node labels.
#' @param table.column Name of Cytoscape table column to map values from
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelMapping('name')
#' }
#' @export
setNodeLabelMapping <- function (table.column, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) return()
    mvp <- mapVisualProperty("NODE_LABEL", table.column, 'p', 
                             network = network, base.url = base.url)  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}


# ------------------------------------------------------------------------------
#' @title Set Node Label Color Mapping
#'
#' @description Map table column values to colors to set the node border color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setNodeLabelColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                       mapping.type='c', default.color=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setNodeLabelColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_LABEL_COLOR",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_LABEL_COLOR",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_LABEL_COLOR",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Label Opacity Mapping
#'
#' @description Sets opacity for node label only.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped 
#' values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelOpacityMapping('score', c(-5,5), c(50,255))
#' }
#' @export
setNodeLabelOpacityMapping <- function(table.column, table.column.values=NULL, 
                                        opacities=NULL, mapping.type='c', 
                                        default.opacity=NULL, style.name=NULL, 
                                        network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    for (o in opacities){
        .checkOpacity(o)
    }
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity)
        setVisualPropertyDefault(
            list(visualProperty = "NODE_LABEL_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp1 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("NODE_LABEL_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Node Shape Mapping
#'
#' @description Map table column values to shapes to set the node shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param shapes List of shapes to map to table.column.values. See \link{getNodeShapes}
#' @param default.shape Shape to set as default. See \link{getNodeShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeShapeMapping('type',c('protein','dna'),c('ELLIPSE','RECTANGLE'))
#' }
#' @export
setNodeShapeMapping <- function (table.column, table.column.values, shapes, 
                                 default.shape=NULL , style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setNodeShapeDefault(default.shape, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("NODE_SHAPE",table.column, 'd',
                             table.column.values, shapes, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Size Mapping
#'
#' @description Map table column values to node font sizes.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param sizes List of font size values to mape to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.size Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                mapping.type='c', default.size=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.size))
        setNodeSizeDefault(default.size, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_SIZE",table.column, 'c',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_SIZE",table.column, 'd',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_SIZE",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Tooltip Mapping
#'
#' @description Pass the values from a table column to display as node tooltips.
#' @param table.column Name of Cytoscape table column to map values from
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeTooltipMapping('description')
#' }
#' @export
setNodeTooltipMapping <- function (table.column, style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'node',network, base.url)) return()
    mvp <- mapVisualProperty("NODE_TOOLTIP", table.column, 'p', 
                             network = network, base.url = base.url)  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Width Mapping
#'
#' @description Map table column values to the node widths.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param widths List of width values to mape to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.width Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeWidthMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                  mapping.type='c', default.width=NULL, style.name=NULL, 
                                  network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.width))
        setNodeWidthDefault(default.width, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_WIDTH",table.column, 'c',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_WIDTH",table.column, 'd',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("NODE_WIDTH",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}
# ==============================================================================
# II.b. Edge Properties
# Pattern: (1) prepare mapVisualProperty, (2) call updateStyleMapping()
# ------------------------------------------------------------------------------
#' @title Set Edge Color Mapping
#'
#' @description Map table column values to colors to set the edge color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors List of hex colors to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setEdgeColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                 mapping.type='c', default.color=NULL, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setEdgeColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping for COLOR (i.e., when arrowColorMatchesEdge=T)
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_UNSELECTED_PAINT",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_UNSELECTED_PAINT",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_UNSELECTED_PAINT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
    
    # perform mapping for STROKE (i.e., when arrowColorMatchesEdge=F)
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_STROKE_UNSELECTED_PAINT",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_STROKE_UNSELECTED_PAINT",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_STROKE_UNSELECTED_PAINT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
    
} 

# ------------------------------------------------------------------------------
#' @title Set Edge Font Face Mapping
#'
#' @description Sets font face for edge labels.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param fonts List of string specifications of font face, style and size, e.g., 
#' c("SansSerif,plain,12", "Dialog,plain,10")
#' @param mapping.type (char) discrete or passthrough (d,p); 
#' default is discrete
#' @param default.font String specification of font face, style and size, e.g., 
#' "SansSerif,plain,12" or "Dialog,plain,10"
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeFontFaceMapping("myfonts", c("normal","small"),
#' c("SansSerif,plain,12", "Dialog,plain,10"))
#' }
#' @export
setEdgeFontFaceMapping <- function(table.column, table.column.values, 
                                   fonts, mapping.type='d', 
                                   default.font=NULL,
                                   style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'edge',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    if(!is.null(default.font)){
        setVisualPropertyDefault(
            list(visualProperty = "EDGE_LABEL_FONT_FACE", value = default.font), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        stop("Continuous mapping of font face values is not supported.")
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("EDGE_LABEL_FONT_FACE",table.column, 'd',
                                  table.column.values, fonts, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("EDGE_LABEL_FONT_FACE",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Mapping
#'
#' @description Map table column values to sizes to set the edge size.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param sizes List of size values to mape to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.size Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeFontSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setEdgeFontSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                    mapping.type='c', default.size=NULL, style.name=NULL, 
                                    network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.size))
        setEdgeFontSizeDefault(default.size, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_LABEL_FONT_SIZE",table.column, 'c',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_LABEL_FONT_SIZE",table.column, 'd',
                                 table.column.values, sizes, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_LABEL_FONT_SIZE",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Edge Label Mapping
#'
#' @description Pass the values from a table column to display as edge labels.
#' @param table.column Name of Cytoscape table column to map values from
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLabelMapping('label')
#' }
#' @export
setEdgeLabelMapping <- function (table.column, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column, 'edge',network, base.url)) return()
    mvp <- mapVisualProperty("EDGE_LABEL", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}
#' @export
# ------------------------------------------------------------------------------
#' @title Set Edge Label Color Mapping
#'
#' @description Map table column values to colors to set the edge border color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLabelColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setEdgeLabelColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                      mapping.type='c', default.color=NULL, style.name=NULL, 
                                      network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setEdgeLabelColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_LABEL_COLOR",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_LABEL_COLOR",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_LABEL_COLOR",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Label Opacity Mapping
#'
#' @description Sets opacity for edge label only.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in 
#' mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped 
#' values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLabelOpacityMapping ('weight', c(1,10), c(50,255))
#' }
#' @export
setEdgeLabelOpacityMapping <- function(table.column, table.column.values=NULL, 
                                   opacities=NULL, mapping.type='c', 
                                   default.opacity=NULL,
                                   style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column,'edge',network, base.url)) 
        stop("Table column does not exist. Please try again.")
    for (o in opacities){
        .checkOpacity(o)
    }
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity)
        setVisualPropertyDefault(
            list(visualProperty = "EDGE_LABEL_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp1 <- mapVisualProperty("EDGE_LABEL_TRANSPARENCY",table.column, 'c',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp1 <- mapVisualProperty("EDGE_LABEL_TRANSPARENCY",table.column, 'd',
                                  table.column.values, opacities, 
                                  network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp1 <- mapVisualProperty("EDGE_LABEL_TRANSPARENCY",table.column, 'p',
                                  network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp1, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Mapping
#'
#' @description Map table column values to styles to set the edge style.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param line.styles List of line styles. See \link{getLineStyles}.
#' @param default.line.style Style to set as default. See \link{getLineStyles}.
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineStyleMapping('type',c('pp','pd'),c('SOLID','LONG_DASH'))
#' }
#' @export
setEdgeLineStyleMapping <- function (table.column, table.column.values, line.styles, 
                                     default.line.style='SOLID', style.name=NULL, 
                                     network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.line.style))
        setEdgeLineStyleDefault(default.line.style, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("EDGE_LINE_TYPE",table.column, 'd',
                             table.column.values, line.styles, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Mapping
#'
#' @description Map table column values to widths to set the edge line width.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param widths List of width values to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.width Width value to set as default for all unmapped values for all unmapped values.
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineWidthMapping('score', c(0,30), c(1,5))
#' }
#' @export
setEdgeLineWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                     mapping.type='c', default.width=NULL, style.name=NULL, 
                                     network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.width))
        setEdgeLineWidthDefault(default.width, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_WIDTH",table.column, 'c',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_WIDTH",table.column, 'd',
                                 table.column.values, widths, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_WIDTH",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Opacity Mapping
#'
#' @description Map table column values to opacities to set the edge opacity.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param opacities (integer) values between 0 and 255; 0 is invisible
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.opacity Opacity value to set as default for all unmapped values
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeOpacityMapping ('weight', c(1,10), c(50,255))
#' }
#' @export
setEdgeOpacityMapping <- function (table.column, table.column.values=NULL, 
                                   opacities=NULL, mapping.type='c',
                                   default.opacity=NULL, style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    
    # check if opacities are formatted correctly
    for (o in opacities){
        .checkOpacity(o)
    }
    
    # set default
    if(!is.null(default.opacity)){
        .checkOpacity(default.opacity)
        setVisualPropertyDefault(
            list(visualProperty = "EDGE_TRANSPARENCY", value = default.opacity), 
            style.name, base.url)
    }    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_TRANSPARENCY",table.column, 'c',
                                 table.column.values, opacities, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_TRANSPARENCY",table.column, 'd',
                                 table.column.values, opacities, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_TRANSPARENCY",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 
# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Mapping
#'
#' @description Map table column values to shapes to set the target arrow shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param shapes List of shapes to map to table.column.values. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowMapping('type',c('activation','inhibition'),c('ARROW','T'))
#' }
#' @export
setEdgeTargetArrowMapping <- function (table.column, table.column.values, shapes, 
                                       default.shape='ARROW', style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeTargetArrowShapeDefault(default.shape, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("EDGE_TARGET_ARROW_SHAPE",table.column, 'd',
                             table.column.values, shapes, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Mapping
#'
#' @description Map table column values to shapes to set the source arrow shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param shapes List of shapes to map to table.column.values. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowMapping('type',c('activation','inhibition'),c('ARROW','T'))
#' }
#' @export
setEdgeSourceArrowMapping <- function (table.column, table.column.values, shapes, 
                                       default.shape='ARROW', style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeSourceArrowShapeDefault(default.shape, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("EDGE_SOURCE_ARROW_SHAPE",table.column, 'd',
                             table.column.values, shapes, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}


# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Mapping
#'
#' @description Map table column values to colors to set the target arrow color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors List of hex colors to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setEdgeTargetArrowColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                            mapping.type='c', default.color=NULL, style.name=NULL, 
                                            network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setEdgeTargetArrowColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_TARGET_ARROW_UNSELECTED_PAINT",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_TARGET_ARROW_UNSELECTED_PAINT",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_TARGET_ARROW_UNSELECTED_PAINT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Mapping
#'
#' @description Map table column values to colors to set the source arrow color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param colors List of hex colors to map to table.column.values
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' }
#' @export
setEdgeSourceArrowColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                            mapping.type='c', default.color=NULL, style.name=NULL, 
                                            network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        .checkHexColor(color)
    }
    
    # set default
    if(!is.null(default.color))
        setEdgeSourceArrowColorDefault(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("EDGE_SOURCE_ARROW_UNSELECTED_PAINT",table.column, 'c',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("EDGE_SOURCE_ARROW_UNSELECTED_PAINT",table.column, 'd',
                                 table.column.values, colors, 
                                 network=network, base.url = base.url)
    } else if (mapping.type %in% c('passthrough','p')){
        mvp <- mapVisualProperty("EDGE_SOURCE_ARROW_UNSELECTED_PAINT",table.column, 'p',
                                 network=network, base.url = base.url)
    } else {
        stop("mapping.type not recognized.")
        
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Shape Mapping
#'
#' @description Map table column values to shapes to set the target arrow shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param shapes List of arrow shapes to map to table.column.values. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowShapeMapping('type',c('activation','inhibition'),
#' c('ARROW','T'))
#' }
#' @export
setEdgeTargetArrowShapeMapping <- function (table.column, table.column.values, shapes, 
                                 default.shape=NULL , style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeTargetArrowShapeDefault(default.shape, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("EDGE_TARGET_ARROW_SHAPE",table.column, 'd',
                             table.column.values, shapes, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Shape Mapping
#'
#' @description Map table column values to shapes to set the source arrow shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in mapping
#' @param shapes List of arrow shapes to map to table.column.values. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowShapeMapping('type',c('activation','inhibition'),
#' c('ARROW','T'))
#' }
#' @export
setEdgeSourceArrowShapeMapping <- function (table.column, table.column.values, shapes, 
                                            default.shape=NULL , style.name=NULL, 
                                            network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeSourceArrowShapeDefault(default.shape, style.name, base.url=base.url)
    
    # perform mapping
    mvp <- mapVisualProperty("EDGE_SOURCE_ARROW_SHAPE",table.column, 'd',
                             table.column.values, shapes, 
                             network=network, base.url = base.url)
    
    updateStyleMapping(style.name, mvp, base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Set Edge Tooltip Mapping
#'
#' @description Pass the values from a table column to display as edge tooltips.
#' @param table.column Name of Cytoscape table column to map values from
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTooltipMapping('description')
#' }
#' @export
setEdgeTooltipMapping <- function (table.column, style.name=NULL, 
                                   network=NULL, base.url=.defaultBaseUrl) {
    if(!.tableColumnExists(table.column, 'edge',network, base.url)) return()
    mvp <- mapVisualProperty("EDGE_TOOLTIP", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

