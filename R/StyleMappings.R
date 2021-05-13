# ==============================================================================
# Functions for defining MAPPINGS between table column values and visual 
# properties, organized into sections:
#
# I.   General functions for creating and applying mappings for node, edge and 
#      network properties
# II.  Specific functions for defining particular node, edge and network 
#      properties
# III. Functions for generating and mapping property values to colors, 
#      opacities, sizes, heights, widths and shapes
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

# ------------------------------------------------------------------------------
#' @title Get Style Mapping
#'
#' @description Retrieve the values the define the mappings for a given
#' visual property in a style.
#' @param visual.prop Name of the visual property, e.g., NODE_FILL_COLOR
#' @param style.name Name for style; default is the 'default' style
#' @param base_url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return List of named values defining the visual property mappings
#' @examples \donttest{
#' getStyleMapping()
#' }
#' @seealso getVisualPropertyNames
#' @export
getStyleMapping <- function(visual.prop, style.name=NULL,
                            base.url=.defaultBaseUrl){
    if(is.null(style.name)){
        style.name <- 'default'
        message('style.name not specified, so using "default" style.')
    }
    
    cyrestGET(paste('styles',style.name,'mappings', visual.prop, sep = '/'), 
              base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Get All Style Mappings
#'
#' @description Retrieve the values the define all the mappings per visual 
#' property in a given style.
#' @param style.name Name for style; default is the 'default' style
#' @param base_url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return List of lists with named values defining the visual property mappings
#' @examples \donttest{
#' getStyleMapping()
#' }
#' @export
getAllStyleMappings <- function(style.name=NULL,
                            base.url=.defaultBaseUrl){
    if(is.null(style.name)){
        style.name <- 'default'
        message('style.name not specified, so using "default" style.')
    }
    
    cyrestGET(paste('styles',style.name,'mappings', sep = '/'), 
              base.url=base.url)
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setNodeBorderColorMapping('score', colors=paletteColorBrewerRdBu)
#' setNodeBorderColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setNodeBorderColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                       mapping.type='c', default.color=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('node',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
    
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
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setNodeBorderOpacityMapping('score')
#' setNodeBorderOpacityMapping('score', opacities=c(0,100))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('node',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param widths List of width values to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
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
#' setNodeBorderWidthMapping('score')
#' setNodeBorderWidthMapping('score', widths=c(1,10))
#' setNodeBorderWidthMapping('score', c(0,30), c(1,5))
#' }
#' @export
setNodeBorderWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                       mapping.type='c', default.width=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.width))
        setNodeBorderWidthDefault(default.width, style.name, base.url=base.url)
    
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('node',table.column,widths,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        widths<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeColorMapping('score', c(-5,0,5), c('#5577FF','#FFFFFF','#FF7755'))
#' setNodeColorMapping('score', colors=paletteColorBrewerRdBu)
#' setNodeColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setNodeColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                 mapping.type='c', default.color=NULL, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('node',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
                colors <- colors(length(table.column.values))
        }
    }
    
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
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setNodeComboOpacityMapping('score')
#' setNodeComboOpacityMapping('score', opacities=c(0,100))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('node',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setNodeFillOpacityMapping('score')
#' setNodeFillOpacityMapping('score', opacities=c(0,100))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('node',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param sizes List of sizes to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
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
#' setNodeFontSizeMapping('score')
#' setNodeFontSizeMapping('score', sizes=c(6,24))
#' setNodeFontSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeFontSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                mapping.type='c', default.size=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.size))
        setNodeFontSizeDefault(default.size, style.name, base.url=base.url)
    
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('node',table.column,sizes,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        sizes<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param heights List of height values to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.height Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @details Using this function will unlock node width and height to use  
#' separate values.
#' @examples \donttest{
#' setNodeHeightMapping('score')
#' setNodeHeightMapping('score', heights=c(30,80))
#' setNodeHeightMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeHeightMapping <- function (table.column, table.column.values=NULL, heights=NULL, 
                                mapping.type='c', default.height=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    lockNodeDimensions(FALSE, style.name,base.url)
    
    # set default
    if(!is.null(default.height))
        setNodeHeightDefault(default.height, style.name, base.url=base.url)
    
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('node',table.column,heights,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        heights<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setNodeLabelColorMapping('score', colors=paletteColorBrewerRdBu)
#' setNodeLabelColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setNodeLabelColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                       mapping.type='c', default.color=NULL, style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('node',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
    
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
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setNodeLabelOpacityMapping('score')
#' setNodeLabelOpacityMapping('score', opacities=c(0,100))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('node',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param shapes List of shapes to map to table.column.values. Leave NULL to 
#' perform an automatic mapping to available shapes. See \link{getNodeShapes}
#' @param default.shape Shape to set as default. See \link{getNodeShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeShapeMapping('type')
#' setNodeShapeMapping('type',c('protein','dna'),c('ELLIPSE','RECTANGLE'))
#' }
#' @export
setNodeShapeMapping <- function (table.column, table.column.values=NULL, 
                                 shapes=NULL, 
                                 default.shape=NULL , style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setNodeShapeDefault(default.shape, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genShapeMap(table.column,
                               network,base.url)
        table.column.values<-auto.map$table.column.values
        shapes<-auto.map$shapes
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param sizes List of sizes to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); default is continuous
#' @param default.size Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @details Using this function will lock node width and height to use a 
#' singular "size" value.
#' @examples \donttest{
#' setNodeSizeMapping('score')
#' setNodeSizeMapping('score', sizes=c(30,80))
#' setNodeSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                mapping.type='c', default.size=NULL, style.name=NULL, 
                                network=NULL, base.url=.defaultBaseUrl) {
    
    lockNodeDimensions(TRUE,style.name, base.url)
    
    # set default
    if(!is.null(default.size))
        setNodeSizeDefault(default.size, style.name, base.url=base.url)
    
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('node',table.column,sizes,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        sizes<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param widths List of width values to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.width Size value to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. 
#' Default is the "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' #' @details Using this function will unlock node width and height to use  
#' separate values.
#' @examples \donttest{
#' setNodeWidthMapping('score')
#' setNodeWidthMapping('score', widths=c(30,80))
#' setNodeWidthMapping('score', c(0,30), c(35,55))
#' }
#' @export
setNodeWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                  mapping.type='c', default.width=NULL, style.name=NULL, 
                                  network=NULL, base.url=.defaultBaseUrl) {
    lockNodeDimensions(FALSE, style.name, base.url)
    
    # set default
    if(!is.null(default.width))
        setNodeWidthDefault(default.width, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('node',table.column,widths,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        widths<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setEdgeColorMapping('score', colors=paletteColorBrewerRdBu)
#' setEdgeColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setEdgeColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                 mapping.type='c', default.color=NULL, style.name=NULL, 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('edge',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
    
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param sizes List of sizes to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
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
#' setEdgeFontSizeMapping('score')
#' setEdgeFontSizeMapping('score', sizes=c(6,24))
#' setEdgeFontSizeMapping('score', c(0,30), c(35,55))
#' }
#' @export
setEdgeFontSizeMapping <- function (table.column, table.column.values=NULL, sizes=NULL, 
                                    mapping.type='c', default.size=NULL, style.name=NULL, 
                                    network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.size))
        setEdgeFontSizeDefault(default.size, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('edge',table.column,sizes,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        sizes<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLabelColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setEdgeLabelColorMapping('score', colors=paletteColorBrewerRdBu)
#' setEdgeLabelColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setEdgeLabelColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                      mapping.type='c', default.color=NULL, style.name=NULL, 
                                      network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('edge',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
    
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
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setEdgeLabelOpacityMapping('weight')
#' setEdgeLabelOpacityMapping('weight', opacities=c(0,100))
#' setEdgeLabelOpacityMapping('weight', c(1,10), c(50,255))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('edge',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param line.styles List of line styles. Leave NULL to perform
#' an automatic mapping to available line styles. See \link{getLineStyles}.
#' @param default.line.style Style to set as default. See \link{getLineStyles}.
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineStyleMapping('type')
#' setEdgeLineStyleMapping('type',c('pp','pd'),c('SOLID','LONG_DASH'))
#' }
#' @export
setEdgeLineStyleMapping <- function (table.column, table.column.values=NULL, 
                                     line.styles=NULL, 
                                     default.line.style='SOLID', style.name=NULL, 
                                     network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.line.style))
        setEdgeLineStyleDefault(default.line.style, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genLineStyleMap(table.column,
                                 network,base.url)
        table.column.values<-auto.map$table.column.values
        line.styles<-auto.map$line.styles
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param widths List of width values to map to table.column.values. A range
#' of 10 to 100 is used by default for automatic mapping.
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
#' setEdgeLineWidthMapping('score')
#' setEdgeLineWidthMapping('score', widths=c(1,10))
#' setEdgeLineWidthMapping('score', c(0,30), c(1,5))
#' }
#' @export
setEdgeLineWidthMapping <- function (table.column, table.column.values=NULL, widths=NULL, 
                                     mapping.type='c', default.width=NULL, style.name=NULL, 
                                     network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.width))
        setEdgeLineWidthDefault(default.width, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genDimMap('edge',table.column,widths,mapping.type,
                             network,base.url)
        table.column.values<-auto.map$table.column.values
        widths<-auto.map$dims
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param opacities (integer) values between 0 and 255; 0 is invisible. A range
#' of 50 to 255 is used by default for automatic mapping.
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
#' setEdgeOpacityMapping('weight')
#' setEdgeOpacityMapping('weight', opacities=c(0,100))
#' setEdgeOpacityMapping('weight', c(1,10), c(50,255))
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
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genOpacityMap('edge',table.column,opacities,mapping.type,
                            network,base.url)
        table.column.values<-auto.map$table.column.values
        opacities<-auto.map$opacities
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param shapes List of shapes to map to table.column.values. Leave NULL to 
#' perform an automatic mapping to available shapes. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowMapping('type')
#' setEdgeTargetArrowMapping('type',c('activation','inhibition'),c('ARROW','T'))
#' }
#' @export
setEdgeTargetArrowMapping <- function (table.column, table.column.values=NULL, 
                                       shapes=NULL, 
                                       default.shape='ARROW', style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    #Whoops! Duplicate?? TODO: Deprecate?
    setEdgeTargetArrowShapeMapping(table.column, table.column.values, shapes, 
                                   default.shape , style.name, 
                                   network, base.url) 
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Mapping
#'
#' @description Map table column values to shapes to set the source arrow shape.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param shapes List of shapes to map to table.column.values. Leave NULL to 
#' perform an automatic mapping to available shapes. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowMapping('type')
#' setEdgeSourceArrowMapping('type',c('activation','inhibition'),c('ARROW','T'))
#' }
#' @export
setEdgeSourceArrowMapping <- function (table.column, table.column.values=NULL, 
                                       shapes=NULL, 
                                       default.shape='ARROW', style.name=NULL, 
                                       network=NULL, base.url=.defaultBaseUrl) {
    #Whoops! Duplicate?? TODO: Deprecate?
    setEdgeSourceArrowShapeMapping(table.column, table.column.values, shapes, 
                                   default.shape , style.name, 
                                   network, base.url) 
}


# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Mapping
#'
#' @description Map table column values to colors to set the target arrow color.
#' @param table.column Name of Cytoscape table column to map values from
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setEdgeTargetArrowColorMapping('score', colors=paletteColorBrewerRdBu)
#' setEdgeTargetArrowColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setEdgeTargetArrowColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                            mapping.type='c', default.color=NULL, style.name=NULL, 
                                            network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('edge',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
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
#' @param table.column.values List of values from Cytoscape table to be used in
#' mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param colors List of hex colors to map to table.column.values or a color
#' palette function, e.g., paletteColorBrewerSet3 (without quotes). See 
#' RColorBrewer::display.brewer.all()
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#' default is continuous
#' @param default.color Hex color to set as default
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
#' setEdgeSourceArrowColorMapping('score', colors=paletteColorBrewerRdBu)
#' setEdgeSourceArrowColorMapping('score', colors=paletteColorBrewerSet3, mapping.type='d')
#' }
#' @export
setEdgeSourceArrowColorMapping <- function (table.column, table.column.values=NULL, colors=NULL, 
                                            mapping.type='c', default.color=NULL, style.name=NULL, 
                                            network=NULL, base.url=.defaultBaseUrl) {
    # check for color palette use case
    if(typeof(colors) == "closure"){ #i.e., name of palette function
        if(is.null(table.column.values)){
            auto.map<-.genColorMap('edge',table.column,colors,mapping.type,
                              network,base.url)
            table.column.values<-auto.map$table.column.values
            colors<-auto.map$colors
        } else {
            colors <- colors(length(table.column.values))
        }
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param shapes List of shapes to map to table.column.values. Leave NULL to 
#' perform an automatic mapping to available shapes. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowShapeMapping('type')
#' setEdgeTargetArrowShapeMapping('type',c('activation','inhibition'),
#'   c('ARROW','T'))
#' }
#' @export
setEdgeTargetArrowShapeMapping <- function(table.column, 
                          table.column.values=NULL, shapes=NULL, 
                          default.shape=NULL , style.name=NULL, 
                          network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeTargetArrowShapeDefault(default.shape, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genArrowMap(table.column,
                                   network,base.url)
        table.column.values<-auto.map$table.column.values
        shapes<-auto.map$shapes
    }
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
#' @param table.column.values List of values from Cytoscape table to be used 
#' in mapping. Leave NULL to perform an automatic mapping to all column values.
#' @param shapes List of shapes to map to table.column.values. Leave NULL to 
#' perform an automatic mapping to available shapes. See \link{getArrowShapes}
#' @param default.shape Shape to set as default. See \link{getArrowShapes}
#' @param style.name Name of style; default is "default" style
#' @param network (optional) Name or SUID of the network. Default is the "current" 
#' network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowShapeMapping('type')
#' setEdgeSourceArrowShapeMapping('type',c('activation','inhibition'),
#'   c('ARROW','T'))
#' }
#' @export
setEdgeSourceArrowShapeMapping <- function(table.column, 
                               table.column.values=NULL, shapes=NULL, 
                               default.shape=NULL , style.name=NULL, 
                               network=NULL, base.url=.defaultBaseUrl) {
    # set default
    if(!is.null(default.shape))
        setEdgeSourceArrowShapeDefault(default.shape, style.name, base.url=base.url)
    # check table.column.values to map
    if(is.null(table.column.values)){
        auto.map<-.genArrowMap(table.column,
                                   network,base.url)
        table.column.values<-auto.map$table.column.values
        shapes<-auto.map$shapes
    }
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

# ==============================================================================
# III. Palette property value generators
# ------------------------------------------------------------------------------
#'@export
# ------------------------------------------------------------------------------
#' @title paletteColorRandom Qualitative
#'
#' @description Generate a qualitative random color map of a given size
#' @param value.count Number of colors to generate; default is 1
#' @return List of random colors
#' @examples \donttest{
#' paletteColorRandom()
#' }
#' @seealso genNodeColorMap genEdgeColorMap
#' @export
paletteColorRandom<-function(value.count=1){
    rgb(sample(0:255,size=value.count,replace=TRUE), 
        sample(0:255,size=value.count,replace=TRUE), 
        sample(0:255,size=value.count,replace=TRUE), 
        maxColorValue=255)
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPastel2 Qualitative
#'
#' @description Generate a qualitative Pastel2 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 8
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPastel2()
#' }
#' @export
paletteColorBrewerPastel2<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Pastel2'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPastel1 Qualitative
#'
#' @description Generate a qualitative Pastel1 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPastel1()
#' }
#' @export
paletteColorBrewerPastel1<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Pastel1'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerDark2 Qualitative
#'
#' @description Generate a qualitative Dark2 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 8
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerDark2()
#' }
#' @export
paletteColorBrewerDark2<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Dark2'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerAccent Qualitative
#'
#' @description Generate a qualitative Accent Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 8
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerAccent()
#' }
#' @export
paletteColorBrewerAccent<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Accent'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPaired Qualitative
#'
#' @description Generate a qualitative Paired Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 
#' 12. See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPaired()
#' }
#' @export
paletteColorBrewerPaired<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Paired'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerSet1 Qualitative
#'
#' @description Generate a qualitative Set1 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerSet1()
#' }
#' @export
paletteColorBrewerSet1<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Set1'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerSet2 Qualitative
#'
#' @description Generate a qualitative Set2 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 8
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerSet2()
#' }
#' @export
paletteColorBrewerSet2<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Set2'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerSet3 Qualitative
#'
#' @description Generate a qualitative Set3 Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 
#' 12. See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerSet3()
#' }
#' @export
paletteColorBrewerSet3<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Set3'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerYlOrRd Sequential
#'
#' @description Generate a sequential YlOrRd Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerYlOrRd()
#' }
#' @export
paletteColorBrewerYlOrRd<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'YlOrRd'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerYlOrBr Sequential
#'
#' @description Generate a sequential YlOrBr Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerYlOrBr()
#' }
#' @export
paletteColorBrewerYlOrBr<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'YlOrBr'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerYlGnBu Sequential
#'
#' @description Generate a sequential YlGnBu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerYlGnBu()
#' }
#' @export
paletteColorBrewerYlGnBu<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'YlGnBu'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerYlGn Sequential
#'
#' @description Generate a sequential YlGn Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerYlGn()
#' }
#' @export
paletteColorBrewerYlGn<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'YlGn'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerReds Sequential
#'
#' @description Generate a sequential Reds Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerReds()
#' }
#' @export
paletteColorBrewerReds<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Reds'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerRdPu Sequential
#'
#' @description Generate a sequential RdPu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerRdPu()
#' }
#' @export
paletteColorBrewerRdPu<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'RdPu'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPurples Sequential
#'
#' @description Generate a sequential Purples Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPurples()
#' }
#' @export
paletteColorBrewerPurples<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Purples'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPuRd Sequential
#'
#' @description Generate a sequential PuRd Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPuRd()
#' }
#' @export
paletteColorBrewerPuRd<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'PuRd'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPuBuGn Sequential
#'
#' @description Generate a sequential PuBuGn Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPuBuGn()
#' }
#' @export
paletteColorBrewerPuBuGn<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'PuBuGn'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPuBu Sequential
#'
#' @description Generate a sequential PuBu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPuBu()
#' }
#' @export
paletteColorBrewerPuBu<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'PuBu'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerOrRd Sequential
#'
#' @description Generate a sequential OrRd Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerOrRd()
#' }
#' @export
paletteColorBrewerOrRd<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'OrRd'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerOranges Sequential
#'
#' @description Generate a sequential Oranges Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerOranges()
#' }
#' @export
paletteColorBrewerOranges<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Oranges'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerGreys Sequential
#'
#' @description Generate a sequential Greys Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerGreys()
#' }
#' @export
paletteColorBrewerGreys<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Greys'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerGreens Sequential
#'
#' @description Generate a sequential Greens Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerGreens()
#' }
#' @export
paletteColorBrewerGreens<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Greens'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerGnBu Sequential
#'
#' @description Generate a sequential GnBu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerGnBu()
#' }
#' @export
paletteColorBrewerGnBu<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'GnBu'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerBuPu Sequential
#'
#' @description Generate a sequential BuPu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerBuPu()
#' }
#' @export
paletteColorBrewerBuPu<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'BuPu'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerBuGn Sequential
#'
#' @description Generate a sequential BuGn Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerBuGn()
#' }
#' @export
paletteColorBrewerBuGn<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'BuGn'))
}

# ------------------------------------------------------------------------------
#' @title paletteColorBrewerBlues Sequential
#'
#' @description Generate a sequential Blues Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerBlues()
#' }
#' @export
paletteColorBrewerBlues<-function(value.count=3) {
    return(RColorBrewer::brewer.pal(value.count, 'Blues'))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerRdYlBu Divergent
#'
#' @description Generate a divergent RdYlBu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerRdYlBu()
#' }
#' @export
paletteColorBrewerRdYlBu<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'RdYlBu')))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerRdBu Divergent
#'
#' @description Generate a divergent RdBu Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerRdBu()
#' }
#' @export
paletteColorBrewerRdBu<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'RdBu')))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPuOr Divergent
#'
#' @description Generate a divergent PuOr Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPuOr()
#' }
#' @export
paletteColorBrewerPuOr<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'PuOr')))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPRGn Divergent
#'
#' @description Generate a divergent PRGn Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPRGn()
#' }
#' @export
paletteColorBrewerPRGn<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'PRGn')))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerPiYG Divergent
#'
#' @description Generate a divergent PiYG Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerPiYG()
#' }
#' @export
paletteColorBrewerPiYG<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'PiYG')))
}
# ------------------------------------------------------------------------------
#' @title paletteColorBrewerBrBG Divergent
#'
#' @description Generate a divergent BrBG Brewer palette of a given size
#' @param value.count Number of colors to generate; min is 3 (default); max is 9
#' . See RColorBrewer::display.brewer.all()
#' @return List of palette colors
#' @examples \donttest{
#' paletteColorBrewerBrBG()
#' }
#' @export
paletteColorBrewerBrBG<-function(value.count=3) {
    return(rev(RColorBrewer::brewer.pal(value.count, 'BrBG')))
}

# ==============================================================================
# III.a Internal mapping generators
# ------------------------------------------------------------------------------

# Automatic construction of table.column.values for mapping
.autoTableColumnValues<-function(table=NULL,
                                 table.column=NULL,
                                 mapping.type=NULL,
                                 network=NULL,
                                 base.url=.defaultBaseUrl){
    # Find out all of the values in the named column
    df.values = getTableColumns(table=table, columns=table.column, 
                                network=network, base.url=base.url)
    
    if(mapping.type == 'd'){
        # Find the frequency distribution, with most common elements first 
        # Note: not same ordering as Cytoscape
        df.freq = table(df.values[table.column])
        
        # Create the mapped values that correspond to the unique elements
        return(names(df.freq))
    } else if (mapping.type == 'c'){
        #Determine min and max
        max.value <- max(df.values,na.rm=TRUE)
        min.value <- min(df.values,na.rm=TRUE)
        if(sign(max.value) == sign(min.value)){ #one-tailed; seq
            mid.value = min.value + (max.value - min.value)/2
            return(c(min.value,mid.value,max.value))
        } else {  #two-tailed; div
            #Find max abs value
            max.max.value <- max(max.value, abs(min.value))
            return(c(-max.max.value, 0, max.max.value))
        }
    } else {
        stop("Automatic palette mapping doesn't work for passthrough mappings.")
    }
}

# Internal function to generate a color map
# param table Node, Edge or Network table
# param table.column Name of column
# param color.palette Name of palette function (without quotes)
# param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#  default is continuous
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and colors
.genColorMap<-function(table=NULL,
                      table.column=NULL,
                      color.palette=NULL,
                      mapping.type=NULL,
                      network=NULL,
                      base.url=.defaultBaseUrl){

    table.column.values <- .autoTableColumnValues(table, table.column,
                                                  mapping.type,network, base.url)
    colors <- color.palette(length(table.column.values))
    
    return (list('table.column.values'=table.column.values, 'colors'=colors))
}


# ------------------------------------------------------------------------------
# Internal function to generate an opacity map
# param table Node, Edge or Network table
# param table.column Name of column
# param opacities Range of opacity values to work with; default is 50-255
# param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#  default is continuous
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and opacities
.genOpacityMap<-function(table=NULL,
                           table.column=NULL,
                           opacities=NULL,
                           mapping.type=NULL,
                           network=NULL,
                           base.url=.defaultBaseUrl){
    table.column.values <- .autoTableColumnValues(table, table.column,
                                                  mapping.type,network, base.url)
    if(is.null(opacities))
        opacities <- c(50,255) #default range
    if(length(opacities) > 2){
        warning("More than 2 opacity values provided. Only using min and max.")
        opacities <- c(min(opacities), max(opacities))
    }else if (length(opacities) < 2){
        stop("Please provide min and max opacity values between from 0 to 255")
    }
    
    opacities <- seq(min(opacities), max(opacities), along.with = table.column.values)
    
    return (list('table.column.values'=table.column.values, 'opacities'=opacities))
}


# ------------------------------------------------------------------------------
# Internal function to generate a dimensions map
# param table Node, Edge or Network table
# param table.column Name of column
# param dims Range of dimension values to work with; default is 10-100
# param mapping.type (char) continuous, discrete or passthrough (c,d,p); 
#  default is continuous
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and 
#   dimension values
.genDimMap<-function(table=NULL,
                     table.column=NULL,
                     dims=NULL,
                     mapping.type=NULL,
                     network=NULL,
                     base.url=.defaultBaseUrl){
    
    table.column.values <- .autoTableColumnValues(table, table.column,
                                                  mapping.type,network, base.url)
    if(is.null(dims))
        dims <- c(10,100) #default range
    if(length(dims) > 2){
        warning("More than 2 dimension values provided. Only using min and max.")
        dims <- c(min(dims), max(dims))
    }else if (length(dims) < 2){
        stop("Please provide min and max dimension values.")
    }
    
    dims <- seq(min(dims), max(dims), along.with = table.column.values)
    
    return (list('table.column.values'=table.column.values, 'dims'=dims))
    }


# ------------------------------------------------------------------------------
# Internal function to generate a line style map
# param table.column Name of column
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and line 
#  styles
.genLineStyleMap<-function(table.column=NULL,
                           network=NULL,
                           base.url=.defaultBaseUrl){
    
    table.column.values <- .autoTableColumnValues('edge', table.column,
                                                  'd',network, base.url)
    value.count <- length(table.column.values)
    
    if(value.count > 16)
        stop("A maximum of 16 line styles are available.")

    line.styles <- getLineStyles()[1:value.count]
    
    return (list('table.column.values'=table.column.values, 'line.styles'=line.styles))
}
    
# ------------------------------------------------------------------------------
# Internal function to generate an arrow shape map
# param table.column Name of column
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and arrow
#  shapes
.genArrowMap<-function(table.column=NULL,
                       network=NULL,
                       base.url=.defaultBaseUrl){
    
    table.column.values <- .autoTableColumnValues('edge', table.column,
                                                  'd',network, base.url)
    value.count <- length(table.column.values)
    
    if(value.count > 22)
        stop("A maximum of 22 ArrowShapes are available.")
    
    shapes <- getArrowShapes()[1:value.count]
    
    return (list('table.column.values'=table.column.values, 'shapes'=shapes))
}

# ------------------------------------------------------------------------------
# Internal function to generate a shape map
# param table.column Name of column
# param network (optional) Name or SUID of the network. Default is the 
#  "current" network active in Cytoscape.
# param base.url (optional) Ignore unless you need to specify a custom domain,
#  port or version to connect to the CyREST API. Default is 
#  http://localhost:1234 and the latest version of the CyREST API supported by 
#  this version of RCy3.
# return A named list of lists providing a mapping of column values and shapes
.genShapeMap<-function(table.column=NULL,
                       network=NULL,
                       base.url=.defaultBaseUrl){
    
    table.column.values <- .autoTableColumnValues('node', table.column,
                                                  'd',network, base.url)
    value.count <- length(table.column.values)
    
    if(value.count > 9)
        stop("A maximum of 9 shapes are available.")
    
    shapes <- getNodeShapes()[1:value.count]
    
    return (list('table.column.values'=table.column.values, 'shapes'=shapes))
}

