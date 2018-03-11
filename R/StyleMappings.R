# ------------------------------------------------------------------------------
#' Creates a mapping between an attribute and a visual property
#'
#' @description Generates the appropriate data structure for the "mapping" parameter
#' in setStyleMappings and createStyle.
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
#' @seealso setStyleMappings createStyle
#' @examples
#' \donttest{
#' mapVisualProperty('node fill color','score','c',c(-4.0,0.0,9.0),c('#99CCFF','#FFFFFF','#FF7777'))
#' mapVisualProperty('node shape','type','d',c('protein','metabolite'),c('ellipse','rectangle'))
#' mapVisualProperty('node label','alias','p')
#' }
#' @section List of visual properties:
#' \tabular{lll}{
#' Node Border Line Type \tab Edge Bend \tab Network Background Paint \cr
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
    
    suid <- getNetworkSuid(network)
    
    #process mapping type
    mapping.type.name = switch(mapping.type, 'c'='continuous','d'='discrete','p'='passthrough',mapping.type)
    
    #processs visual property, including common alternatives for vp names :)
    visual.prop.name = toupper(gsub("\\s+","_",visual.prop))
    visual.prop.name = switch(visual.prop.name,
                              'EDGE_COLOR'='EDGE_STROKE_UNSELECTED_PAINT',
                              'EDGE_THICKNESS'='EDGE_WIDTH',
                              'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                              visual.prop.name)
    
    #check mapping column and get type
    tp = tolower(strsplit(visual.prop.name,"_")[[1]][1])
    table = paste0('default',tp)
    res <- cyrestGET(paste('networks',suid,'tables',table,'columns',sep='/'),base.url=base.url)
    table.column.type = NULL
    for(i in 1:length(res)){
        if(res[[i]]$name==table.column){
            table.column.type = res[[i]]$type
            break
        }
    }
    if(is.null(table.column.type))
        print(paste0('Error: Could not find ',table.column,' column in ',table,' table.'))
    
    #construct visual property map
    visual.prop.map <- list(
        mappingType=mapping.type.name,
        mappingColumn=table.column,
        mappingColumnType=table.column.type,
        visualProperty=visual.prop.name
    )
    
    if(mapping.type.name=='discrete'){
        map <- list()
        for (i in 1:length(table.column.values)) {
            map[[i]] <- list(key=table.column.values[i], value=visual.prop.values[i])
        }
        visual.prop.map$map=map
    }else if(mapping.type.name=='continuous'){
        points <- list()
        #check for extra lesser and greater values
        propValCount <- length(visual.prop.values)
        colValCount <- length(table.column.values)
        if (propValCount-colValCount == 2){
            for (i in 1:colValCount) {
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
            for (i in 1:colValCount) {
                points[[i]] <- list(value=table.column.values[i],
                                    lesser=visual.prop.values[i],
                                    equal=visual.prop.values[i],
                                    greater=visual.prop.values[i])
            }
        }
        else {
            write ("Error! RCy3:mapVisualProperty. table.column.values and visual.prop.values don't match up.", stderr ())
            return ()
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
        invisible(cyrestPUT(paste('styles', style.name,'mappings',visual.prop.name, sep = '/'),
                      body=list(mapping), 
                      base.url=base.url))
    }
    else {   
        invisible(cyrestPOST(paste('styles', style.name,'mappings',sep = '/'),
                       body=list(mapping), 
                       base.url=base.url))
    }
}


#========================================================================================================================
# Individual Properties
#==========================

# ------------------------------------------------------------------------------
#' @title Set Node Tooltip Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeTooltipMapping()
#' }
#' @export
setNodeTooltipMapping <- function (table.column, style.name = 'default', 
                                   network=NULL, base.url=.defaultBaseUrl) {
    id <- getNetworkSuid(network)
    if(!.tableColumnExists(table.column,'node',network, base.url)) return()
    mvp <- mapVisualProperty("NODE_TOOLTIP", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Tooltip Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTooltipMapping()
#' }
#' @export
setEdgeTooltipMapping <- function (edge.attribute.name, style.name = 'default', 
                                   network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    if(!.tableColumnExists(table.column, 'edge',network, base.url)) return()
    mvp <- mapVisualProperty("EDGE_TOOLTIP", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Label Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeLabelMapping()
#' }
#' @export
setNodeLabelMapping <- function (table.column, style.name = 'default', 
                                 network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    if(!.tableColumnExists(table.column,'node',network, base.url)) return()
    mvp <- mapVisualProperty("NODE_LABEL", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Label Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLabelMapping()
#' }
#' @export
setEdgeLabelMapping <- function (edge.attribute.name, style.name = 'default', 
                                    network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    if(!.tableColumnExists(table.column, 'edge',network, base.url)) return()
    mvp <- mapVisualProperty("EDGE_LABEL", table.column, 'p')  
    updateStyleMapping(style.name, mvp, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Color Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param colors DESCRIPTION
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p)
#' @param default.color DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeColorMapping()
#' }
#' @export
setNodeColorMapping <- function (table.column, table.column.values, colors, 
                                 mapping.type='c', default.color=NULL, style.name='default', 
                                 network=NULL, base.url=.defaultBaseUrl) {
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)) return()
    }
    
    # set default
    if(!is.null(default.color))
        setDefaultNodeColor(default.color, style.name, base.url=base.url)
    
    # perform mapping
    if (mapping.type %in% c('continuous','c','interpolate')) {
        mvp <- mapVisualProperty("NODE_FILL_COLOR",table.column, 'c',
                          table.column.values, colors, 
                          network=network, base.url = base.url)
    } else if (mapping.type %in% c('discrete','d','lookup')){
        mvp <- mapVisualProperty("NODE_FILL_COLOR",table.column, 'd',
                          table.column.values, colors, 
                          network=network, base.url = base.url)

    } else {
        write(print("mapping.type not recognized."), stderr())
        return()
    }
    updateStyleMapping(style.name, mvp, base.url = base.url)
} 

#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

# ------------------------------------------------------------------------------
#' @title Set Node Opacity Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param opacities DESCRIPTION
#' @param mode DESCRIPTION
#' @param aspect DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeOpacityMapping()
#' }
#' @export
setNodeOpacityMapping <- function (table.column, table.column.values, opacities, mode, aspect='all', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeOpacityMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    # set default # Comment TanjaM: Current version does not set default
    #setDefaultNodeOpacity (default.opacity, style.name)
    
    aspect.all = length (grep ('all', aspect))  > 0
    aspect.fill = length (grep ('fill', aspect)) > 0
    aspect.border = length (grep ('border', aspect)) > 0
    aspect.label = length (grep ('label', aspect)) > 0
    
    if (aspect.all) {
        aspect.fill = TRUE
        aspect.border = TRUE
        aspect.label = TRUE
    }
    
    if (aspect.fill == FALSE && aspect.border == FALSE && aspect.label == FALSE) {
        specific.options = 'fill, border, label'
        msg.1 = "Error! RCy3:setNodeOpacityMapping. Aspect must be 'all' (the default) "
        msg.2 = sprintf ("or some combination, in any order, of %s", specific.options)
        write (msg.1, stderr ())
        write (msg.2, stderr ())
        return ()
    }
    
    if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than table.column.values 
        if (length (table.column.values) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
            opacities = c (opacities [1], opacities, opacities [length (opacities)])
            write ("RCy3::setNodeOpacityMapping, no 'below' or 'above' opacities specified.  Inferred from supplied opacities.", stderr ());
        }
        
        good.args = length (table.column.values) == (length (opacities) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setNodeOpacityMapping, interpolate mode.", stderr ())
            write ("Expecting 1 opacity for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
            return ()
        }
        
        if (aspect.fill){
            continuousMapping (table.column, table.column.values, opacities,
                               visual.property="NODE_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
        if (aspect.border){
            continuousMapping (table.column, table.column.values, opacities,
                               visual.property="NODE_BORDER_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
        if (aspect.label){
            continuousMapping (table.column, table.column.values, opacities,
                               visual.property="NODE_LABEL_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
    } # if mode==interpolate
    
    else { # mode==lookup, use a discrete Mapping, with no interpolation
        good.args = length (table.column.values) == length (opacities)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setNodeOpacityMapping.  Expecting exactly as many opacities as table.column.values in lookup mode.", stderr ())
            return ()
        }
        
        default.opacity = 255;
        if (length (table.column.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
            table.column.values = rep (table.column.values, 2)
            opacities = rep (opacities, 2)
        }
        
        if (aspect.fill){
            discreteMapping(table.column, table.column.values, opacities,
                            visual.property="NODE_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
        
        if (aspect.border){
            discreteMapping(table.column, table.column.values, opacities,
                            visual.property="NODE_BORDER_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
        
        if (aspect.label){
            discreteMapping(table.column, table.column.values, opacities,
                            visual.property="NODE_LABEL_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
    } # else: !interpolate
} # setNodeOpacityMapping

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param colors DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.color DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeBorderColorMapping()
#' }
#' @export
setNodeBorderColorMapping <- function (table.column, table.column.values, colors, mode, default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeBorderColorMapping. Mode must be 'interpolate' or 'lookup'.", stderr ())
        return ()
    }
    
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)){
            return()
        } 
    }
    
    # set default
    setDefaultNodeBorderColor (default.color, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    # mode==interpolate
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than table.column.values 
        if (length (table.column.values) == length (colors)){  # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (default.color, colors, default.color)
        }
        good.args = length (table.column.values) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeBorderColorMapping, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        # continous mapping
        continuousMapping (table.column, table.column.values, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)
        
    } # if mode==interpolate
    else { # use a discrete Mapping, with no interpolation
        good.args = length (table.column.values) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeBorderColorMapping.  Expecting exactly as many colors as table.column.values in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(table.column, table.column.values, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)
    } # else: !interpolate
} # setNodeBorderColorMapping

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param line.widths DESCRIPTION
#' @param default.width DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeBorderWidthMapping()
#' }
#' @export
setNodeBorderWidthMapping <- function (table.column, attribute.values, line.widths, default.width=1, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    #TODO we should add interpolate as mode in the function
    mode = "lookup"
    
    if (!table.column %in% getTableColumnNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeBorderWidthMapping: passed non-existent node attribute: %s', table.column), stderr ())
        return ()
    }
    
    # set default
    setDefaultNodeBorderWidth(default.width, style.name)
    
    # define the column type
    columnType <- "String" #.findColumnType(typeof(line.widths[1]))
    # discrete mapping
    if (mode=="lookup"){
        discreteMapping (table.column, attribute.values, line.widths,
                         visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
    } else{
        # continuous mapping
        # TODO need to check here if 2 more values were passed in for width
        continuousMapping (table.column, attribute.values, line.widths, visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
    }
}

# ------------------------------------------------------------------------------
#' @title Set Node Shape Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param node.shapes DESCRIPTION
#' @param default.shape DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeShapeMapping()
#' }
#' @export
setNodeShapeMapping <- function (table.column, attribute.values, node.shapes, default.shape='ELLIPSE', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!table.column %in% getTableColumnNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeShapeMapping: passed non-existent node attribute: %s', table.column), stderr ())
        return ()
    }
    
    # ensure correct node shapes
    node.shapes <- toupper(node.shapes)
    unique.node.shapes <- unique(node.shapes)
    wrong.node.shape <- sapply(unique.node.shapes, function(x) !(x %in% getNodeShapes(network=network, base.url=base.url)))
    if (any(wrong.node.shape)){
        write (sprintf('ERROR in RCy3::setNodeShapeMapping. You tried to use invalid node shapes. For valid ones use getNodeShapes'), stderr())
        return(NA)
    }
    
    # set default
    setDefaultNodeShape (default.shape, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(node.shapes[1]))
    
    # discrete mapping
    discreteMapping (table.column, attribute.values, node.shapes,
                     visual.property="NODE_SHAPE", columnType=columnType, style=style.name)
} # setNodeShapeMapping

# ------------------------------------------------------------------------------
#' @title Set Node Size Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param table.column DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param node.sizes DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.size DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setNodeSizeMapping()
#' }
#' @export
setNodeSizeMapping <- function (table.column, table.column.values, node.sizes, mode, default.size=40, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeSizeMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    # lock node dimensions
    lockNodeDimensions (TRUE)
    
    # set default
    setDefaultNodeSize (default.size, style.name)
    
    if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than table.column.values
        if (length (table.column.values) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
            node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
            write ("RCy3::setNodeSizeMapping, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
        }
        
        good.args = length (table.column.values) == (length (node.sizes) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (node.sizes)), stderr ())
            write ("Error! RCy3:setNodeSizeMapping, interpolate mode.", stderr ())
            write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
            return ()
        }
        continuousMapping (table.column, table.column.values, node.sizes,
                           visual.property="NODE_SIZE",
                           columnType=columnType, style=style.name)
        
    } # if mode==interpolate
    
    else { # use a discrete Mapping, with no interpolation
        good.args = length (table.column.values) == length (node.sizes)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (node.sizes)), stderr ())
            write ("Error! RCy3:setNodeSizeMapping. Expecting exactly as many node.sizes as table.column.values in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(table.column, table.column.values, node.sizes,
                        visual.property="NODE_SIZE",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
    
} # setNodeSizeMapping


# ------------------------------------------------------------------------------
#' @title Set Edge Color Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param colors DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.color DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeColorMapping()
#' }
#' @export
setEdgeColorMapping <- function (edge.attribute.name, table.column.values, colors, mode="interpolate", default.color='#FFFFFF', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeColorMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)){
            return()
        } 
    }
    
    #set default
    setDefaultEdgeColor (default.color, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than table.column.values
        if (length (table.column.values) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeColorMapping, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (table.column.values) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorMapping, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, table.column.values, colors,
                           visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete Mapping, with no interpolation, mode==lookup
        good.args = length (table.column.values) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorMapping.  Expecting exactly as many colors as table.column.values in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, table.column.values, colors,
                        visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
} # setEdgeColorMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Opacity Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param opacities DESCRIPTION
#' @param mode DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeOpacityMapping()
#' }
#' @export
setEdgeOpacityMapping <- function (edge.attribute.name, table.column.values, opacities, mode, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeOpacityMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # set default
    setDefaultEdgeOpacity (default.opacity, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    # in a previous Cytoscape version the three elements were set seperately
    #aspects = c ('Edge Opacity', 'Edge Target Arrow Opacity', 'Edge Source Arrow Opacity')
    
    if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than table.column.values 
        if (length (table.column.values) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
            opacities = c (opacities [1], opacities, opacities [length (opacities)])
            write ("RCy3::setEdgeOpacityMapping, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (table.column.values) == (length (opacities) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setEdgeOpacityMapping, interpolate mode.", stderr ())
            write ("Expecting 1 opacity value for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
            return ()
        }
        continuousMapping (edge.attribute.name, table.column.values, opacities,
                           visual.property="EDGE_TRANSPARENCY",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete Mapping, with no interpolation
        good.args = length (table.column.values) == length (opacities)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setEdgeColorMapping.  Expecting exactly as many opacities as table.column.values in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(edge.attribute.name, table.column.values, opacities,
                        visual.property="EDGE_TRANSPARENCY",
                        columnType=columnType, style=style.name)
    } # else: !interpolate
} # setEdgeColorMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param line.styles DESCRIPTION
#' @param default.style DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLineStyleMapping()
#' }
#' @export
setEdgeLineStyleMapping <- function (edge.attribute.name, attribute.values, line.styles, default.style='SOLID', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getTableColumnNames('edge', network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeLineStyleMapping: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # ensure correct values
    line.styles <- toupper(line.styles)
    unique.values <- unique(line.styles)
    wrong.values <- sapply(unique.values, function(x) !(x %in% getLineStyles(network=network, base.url=base.url)))
    if (any(wrong.values)){
        write (sprintf ('ERROR in RCy3::setEdgeLineStyleMapping. Invalid value. For valid values use getLineStyles'), stderr ())
        return(NA)
    }
    
    # set default
    default.style.list <- list(visualProperty = "EDGE_LINE_TYPE", value = default.style)
    setVisualProperty(default.style.list, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(line.styles[1]))
    
    # discrete mapping
    discreteMapping (edge.attribute.name, attribute.values, line.styles,
                     visual.property="EDGE_LINE_TYPE", columnType=columnType, style=style.name)
} # setEdgeLineStyleMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param line.widths DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.width DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeLineWidthMapping()
#' }
#' @export
setEdgeLineWidthMapping <- function (edge.attribute.name, attribute.values, line.widths, mode="interpolate", default.width=1, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getTableColumnNames('edge', network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeLineWidthMapping: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # unconventional arg.name
    table.column.values = attribute.values
    
    # set default
    default.width.list <- list(visualProperty = "EDGE_WIDTH", value = default.width)
    setVisualProperty(default.width.list, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(line.widths[1]))
    #columnType <- 'String'
    
    if (mode=='interpolate') {  # need a 'below' width and an 'above' width  so there should be two more width than table.column.values
        if (length (table.column.values) == length (line.widths)) { # caller did not supply 'below' and 'above' values; manufacture them
            line.widths = c (line.widths [1], line.widths, line.widths [length (line.widths)])
            write ("RCy3::setEdgeLineWidthMapping, no 'below' or 'above' widths specified.  Inferred from supplied widths.", stderr ());
        } 
        good.args = length (table.column.values) == (length (line.widths) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (line.widths)), stderr ())
            write ("Error! RCy3:setEdgeLineWidthMapping, interpolate mode.", stderr ())
            write ("Expecting 1 widths for each control.point, one for 'above' widths, one for 'below' widths", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, table.column.values, line.widths,
                           visual.property="EDGE_WIDTH",
                           columnType=columnType, style=style.name)
    } 
    else { # use a discrete Mapping, with no interpolation, mode==lookup
        good.args = length (table.column.values) == length (line.widths)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (line.widths)), stderr ())
            write ("Error! RCy3:setEdgeLineWidthMapping.  Expecting exactly as many widths as table.column.values in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping (edge.attribute.name, table.column.values, line.widths,
                         visual.property="EDGE_WIDTH", columnType=columnType, style=style.name)
        
    } 
}

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param arrows DESCRIPTION
#' @param default DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTargetArrowMapping()
#' }
#' @export
setEdgeTargetArrowMapping <- function (edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getTableColumnNames('edge', network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeTargetArrowMapping: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # set default
    default.style.list <- list(visualProperty = "EDGE_TARGET_ARROW_SHAPE", value = default)
    setVisualProperty(default.style.list, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(arrows[1]))
    
    # discrete mapping
    discreteMapping (edge.attribute.name, attribute.values, arrows,
                     visual.property="EDGE_TARGET_ARROW_SHAPE", columnType=columnType, style=style.name)
} # setTargetArrowMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param attribute.values DESCRIPTION
#' @param arrows DESCRIPTION
#' @param default DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeSourceArrowMapping()
#' }
#' @export
setEdgeSourceArrowMapping <- function (edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getTableColumnNames('edge', network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeSourceArrowMapping: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # set default
    default.style.list <- list(visualProperty = "EDGE_SOURCE_ARROW_SHAPE", value = default)
    setVisualProperty(default.style.list, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(arrows[1]))
    
    # discrete mapping
    discreteMapping (edge.attribute.name, attribute.values, arrows,
                     visual.property="EDGE_SOURCE_ARROW_SHAPE", columnType=columnType, style=style.name)
} # setTargetArrowMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param colors DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.color DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeTargetArrowColorMapping()
#' }
#' @export
setEdgeTargetArrowColorMapping <- function (edge.attribute.name, table.column.values, colors, mode="interpolate", default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeTargetArrowColorMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)){
            return()
        } 
    }
    
    #set default
    setDefaultEdgeTargetArrowColor (default.color, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than table.column.values
        if (length (table.column.values) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeTargetArrowColorMapping, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (table.column.values) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeTargetArrowColorMapping, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, table.column.values, colors,
                           visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete Mapping, with no interpolation, mode==lookup
        good.args = length (table.column.values) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeTargetArrowColorMapping.  Expecting exactly as many colors as table.column.values in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, table.column.values, colors,
                        visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
} # setTargetArrowMapping

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Mapping
#'
#' @description FUNCTION_DESCRIPTION
#' @param edge.attribute.name DESCRIPTION
#' @param table.column.values DESCRIPTION
#' @param colors DESCRIPTION
#' @param mode DESCRIPTION
#' @param default.color DESCRIPTION
#' @param style.name DESCRIPTION
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' setEdgeSourceArrowColorMapping()
#' }
#' @export
setEdgeSourceArrowColorMapping <- function (edge.attribute.name, table.column.values, colors, mode="interpolate", default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeSourceArrowColorMapping.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)){
            return()
        } 
    }
    
    #set default
    setDefaultEdgeSourceArrowColor (default.color, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(table.column.values[1]))
    
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than table.column.values
        if (length (table.column.values) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeSourceArrowColorMapping, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (table.column.values) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeSourceArrowColorMapping, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, table.column.values, colors,
                           visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete Mapping, with no interpolation, mode==lookup
        good.args = length (table.column.values) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (table.column.values)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeSourceArrowColorMapping.  Expecting exactly as many colors as table.column.values in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, table.column.values, colors,
                        visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
    
} # setEdgeSourceArrowColorMapping


# ------------------------------------------------------------------------------
## TODO: replace with getTableColumnTypes()??
.findColumnType <- function(columnType){
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
.tableColumnExists <- function(table.column, table='node',network=network, base.url=base.url){
    if (!table.column %in% getTableColumnNames(table, network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeTooltipMapping: passed non-existent node attribute: %s', table.column), stderr ())
        return (FALSE)
    }
    return (TRUE)
}