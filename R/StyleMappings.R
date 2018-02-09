# ------------------------------------------------------------------------------
#' Creates a mapping between an attribute and a visual property
#'
#' @description Generates the appropriate data structure for the "mapping" parameter
#' in setStyleMappings and createStyle.
#' @details The paired list of values must be of the same length or mapping will fail.
#' Mapping will also fail if the data type of table.column.values does not match that of
#' the existing table.column. Note that all imported numeric data are stored as Doubles in
#' Cytosacpe tables; and character or mixed data are stored as Strings.
#' @param visual.prop (char) name of visual property to map
#' @param table.column (char) name of table column to map
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p)
#' @param table.column.values (list) list of values paired with visual.prop.values; skip for passthrough mapping
#' @param visual.prop.values (list) list of values paired with table.column.values; skip for passthrough mapping
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (network=network, base.url=base.url) ready to convert into JSON by style mapping operations
#' @export
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
#' 
#' 

mapVisualProperty <- function(visual.prop, table.column, mapping.type, table.column.values,
                              visual.prop.values,  network=NULL, base.url=.defaultBaseUrl){
    
    suid <- getNetworkSuid(network)
    title <- getNetworkName(suid)
    
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
    t.url = paste(base.url,'networks',suid,'tables',table,'columns',sep='/')
    res <- GET(url=t.url)
    t.res <- unname(fromJSON(rawToChar(res$content)))
    table.column.type = NULL
    for(i in 1:length(t.res)){
        if(t.res[[i]]$name==table.column){
            table.column.type = t.res[[i]]$type
            break
        }
    }
    if(is.null(table.column.type))
        print(paste0('Error: Could not find ',table.column,' column in ',table,' table of network: ',title,'.'))
    
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
        for (i in 1:length(table.column.values)) {
            points[[i]] <- list(value=table.column.values[i],
                                lesser=visual.prop.values[i],
                                equal=visual.prop.values[i],
                                greater=visual.prop.values[i])
        }
        visual.prop.map$points=points
    }
    
    return(visual.prop.map)
}


# ------------------------------------------------------------------------------
discreteMapping <- function(attribute.name, control.points, colors, visual.property, columnType, style){
    mapped.content <- apply(cbind(control.points, colors), MARGIN=1,
                            FUN=function(s) {list(key=as.character(unname(s[[1]])), value=as.character(unname(s[[2]])))})
    
    discrete.mapping <- list(mappingType = "discrete", mappingColumn = attribute.name,
                             mappingColumnType = columnType, visualProperty=visual.property,
                             map = mapped.content)
    discrete.mapping.json <-toJSON(list(discrete.mapping))
    resource.uri <- paste(base.url, "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=discrete.mapping.json, encode="json")
    
    # inform the user if the request was a success or failure
    if (request.res$status == 201){
        write (sprintf ('Successfully set rule.'), stdout ())
    }else{
        write (sprintf ('Error. Could not set rule...'), stdout ())
    }
    
    invisible (request.res)
} # discreteMapping

# ------------------------------------------------------------------------------
continuousMapping <- function(attribute.name, control.points, colors, visual.property, columnType, style){
    # continuous mapping
    mapped.content <- apply(cbind(control.points, colors[3:length(colors)-1]), MARGIN=1,
                            FUN=function(s) {list(value=as.character(unname(s[[1]])),
                                                  lesser=as.character(unname(s[[2]])),
                                                  equal=as.character(unname(s[[2]])),
                                                  greater=as.character(unname(s[[2]])))})
    
    # change the attributes values below the minimum and above the maximum
    mapped.content[[1]]$lesser <- colors[1]
    mapped.content[[length(mapped.content)]]$greater <- colors[length(colors)]
    
    continuous.mapping <- list(mappingType = "continuous", mappingColumn = attribute.name,
                               mappingColumnType = columnType, visualProperty=visual.property,
                               points = mapped.content)
    continuous.mapping.json <- toJSON(list(continuous.mapping))
    resource.uri <- paste(base.url, "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=continuous.mapping.json, encode="json")
    
    # inform the user if the request was a success or failure
    if (request.res$status == 201){
        write (sprintf ('Successfully set rule.'), stdout ())
    }else{
        write (sprintf ('Error. Could not set rule...'), stdout ())
    }
    invisible (request.res)
} # continuousMapping

# ------------------------------------------------------------------------------
#' Updates a visual property mapping in a style
#'
#' @description Updates the visual property mapping, overriding any prior mapping. Creates a
#' visual property mapping if it doesn't already exist in the style.
#' @details Requires visual property mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param mapping a single visual property mapping, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @seealso mapVisualProperty
#' @examples
#' \donttest{
#' updateStyleMapping('myStyle',mapVisualProperty('node label','name','p'))
#' }
#' @import RJSONIO
#' @import httr

updateStyleMapping <- function(style.name, mapping, network=NULL, base.url=.defaultBaseUrl){
    
    base.url=paste(base.url,sep = "/")
    
    visual.prop.name = mapping$visualProperty
    
    # check if vp exists already
    exists = FALSE
    check.url <- URLencode(paste(base.url,'styles', style.name,'mappings',sep = '/'))
    res <- GET(check.url)
    res.elem <- fromJSON(rawToChar(res$content))
    for(re in res.elem){
        if(class(re)=='list')
            if(!is.null(re$visualProperty))
                if(re$visualProperty==visual.prop.name)
                    exists = TRUE
    }
    
    if(exists==TRUE){     #if yes...
        style.url <- URLencode(paste(base.url,'styles', style.name,'mappings',visual.prop.name, sep = '/'))
        map.body <- toJSON(list(mapping))
        invisible(PUT(url=style.url,body=map.body, encode="json"))
        invisible(PUT(url=style.url,body=map.body, encode="json"))    # have to run it twice!? omg...
    }
    else {    # if no...
        style.url <- check.url
        map.body <- toJSON(list(mapping))
        invisible(POST(url=style.url,body=map.body, encode="json"))
    }
}


#========================================================================================================================
# Individual Properties
#==========================

#------------------------------------------------------------------------------------------------------------------------
setNodeTooltipRule <- function (node.attribute.name, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id <- getNetworkSuid(network)
    
    if (!node.attribute.name %in% getNodeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeTooltipRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
        return ()
    }
    
    
    attribute.values <- getTableColumns('node',node.attribute.name)[,node.attribute.name]
    
    # set default tooltip
    default.tooltip <- list(visualProperty = "NODE_TOOLTIP", value = "")
    setVisualProperty(default.tooltip, style.name)
    
    # define the column type
    sample.node.attribute <- getNodeAttribute (getAllNodes(network=network, base.url=base.url)[1], node.attribute.name)
    columnType <- .findColumnType(typeof(sample.node.attribute))
    
    # discrete mapping
    discreteMapping(node.attribute.name, attribute.values, attribute.values,
                    visual.property="NODE_TOOLTIP", columnType=columnType, style=style.name)
    
}  # END setNodeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeTooltipRule <- function (edge.attribute.name, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeTooltipRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    attribute.values <- getTableColumns('edge',edge.attribute.name)[,edge.attribute.name]
    
    # set default tooltip
    default.tooltip <- list(visualProperty = "EDGE_TOOLTIP", value = "")
    setVisualProperty(default.tooltip, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(attribute.values[1]))
    
    # discrete mapping
    discreteMapping(edge.attribute.name, attribute.values, attribute.values,
                    visual.property="EDGE_TOOLTIP", columnType=columnType, style=style.name)
    
}  # setEdgeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setNodeLabelRule <- function (node.attribute.name, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!node.attribute.name %in% getNodeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeLabelRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
        return ()
    }
    
    
    attribute.values <- getTableColumns('node',node.attribute.name)[,node.attribute.name]
    
    # set default label
    default.label <- list(visualProperty = "NODE_LABEL", value = "")
    setVisualProperty(default.label, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(attribute.values[1]))
    
    # discrete mapping
    discreteMapping(node.attribute.name, attribute.values, attribute.values,
                    visual.property="NODE_LABEL", columnType=columnType, style=style.name)
}  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeLabelRule <- function (edge.attribute.name, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeLabelRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    attribute.values <- getTableColumns('edge',edge.attribute.name)[,edge.attribute.name]
    
    # set default label
    default.label <- list(visualProperty = "EDGE_LABEL", value = "")
    setVisualProperty(default.label, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(attribute.values[1]))
    
    # discrete mapping
    discreteMapping(edge.attribute.name, attribute.values, attribute.values,
                    visual.property="EDGE_LABEL", columnType=columnType, style=style.name)
}  # setEdgeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setNodeColorRule <- function (node.attribute.name, control.points, colors, mode, default.color='#FFFFFF', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeColorRule. Mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # check if colors are formatted correctly
    for (color in colors){
        if (.isNotHexColor(color)){
            return()
        } 
    }
    
    # set default
    setDefaultNodeColor (default.color, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(control.points[1]))
    
    # interpolate
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
        if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setNodeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } #
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=style.name)
        
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('control points: %d', length (control.points)), stderr ())
            write (sprintf ('colors: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=style.name)    
        
    } # else: !interpolate, aka lookup
} # setNodeColorRule

#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

setNodeOpacityRule <- function (node.attribute.name, control.points, opacities, mode, aspect='all', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # define the column type
    columnType <- .findColumnType(typeof(control.points[1]))
    
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
        msg.1 = "Error! RCy3:setNodeOpacityRule. Aspect must be 'all' (the default) "
        msg.2 = sprintf ("or some combination, in any order, of %s", specific.options)
        write (msg.1, stderr ())
        write (msg.2, stderr ())
        return ()
    }
    
    if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
        if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
            opacities = c (opacities [1], opacities, opacities [length (opacities)])
            write ("RCy3::setNodeOpacityRule, no 'below' or 'above' opacities specified.  Inferred from supplied opacities.", stderr ());
        }
        
        good.args = length (control.points) == (length (opacities) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setNodeOpacityRule, interpolate mode.", stderr ())
            write ("Expecting 1 opacity for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
            return ()
        }
        
        if (aspect.fill){
            continuousMapping (node.attribute.name, control.points, opacities,
                               visual.property="NODE_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
        if (aspect.border){
            continuousMapping (node.attribute.name, control.points, opacities,
                               visual.property="NODE_BORDER_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
        if (aspect.label){
            continuousMapping (node.attribute.name, control.points, opacities,
                               visual.property="NODE_LABEL_TRANSPARENCY",
                               columnType=columnType, style=style.name)
        }
    } # if mode==interpolate
    
    else { # mode==lookup, use a discrete rule, with no interpolation
        good.args = length (control.points) == length (opacities)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setNodeOpacityRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
            return ()
        }
        
        default.opacity = 255;
        if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
            control.points = rep (control.points, 2)
            opacities = rep (opacities, 2)
        }
        
        if (aspect.fill){
            discreteMapping(node.attribute.name, control.points, opacities,
                            visual.property="NODE_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
        
        if (aspect.border){
            discreteMapping(node.attribute.name, control.points, opacities,
                            visual.property="NODE_BORDER_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
        
        if (aspect.label){
            discreteMapping(node.attribute.name, control.points, opacities,
                            visual.property="NODE_LABEL_TRANSPARENCY",
                            columnType=columnType, style=style.name)
        }
    } # else: !interpolate
} # setNodeOpacityRule

#------------------------------------------------------------------------------------------------------------------------
setNodeBorderColorRule <- function (node.attribute.name, control.points, colors, mode, default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeBorderColorRule. Mode must be 'interpolate' or 'lookup'.", stderr ())
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
    columnType <- .findColumnType(typeof(control.points[1]))
    
    # mode==interpolate
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
        if (length (control.points) == length (colors)){  # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (default.color, colors, default.color)
        }
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeBorderColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        # continous mapping
        continuousMapping (node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)
        
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)
    } # else: !interpolate
} # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setNodeBorderWidthRule <- function (node.attribute.name, attribute.values, line.widths, default.width=1, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    #TODO we should add interpolate as mode in the function
    mode = "lookup"
    
    if (!node.attribute.name %in% getNodeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeBorderWidthRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
        return ()
    }
    
    # set default
    setDefaultNodeBorderWidth(default.width, style.name)
    
    # define the column type
    columnType <- "String" #.findColumnType(typeof(line.widths[1]))
    # discrete mapping
    if (mode=="lookup"){
        discreteMapping (node.attribute.name, attribute.values, line.widths,
                         visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
    } else{
        # continuous mapping
        # TODO need to check here if 2 more values were passed in for width
        continuousMapping (node.attribute.name, attribute.values, line.widths, visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
    }
}

#------------------------------------------------------------------------------------------------------------------------
setNodeShapeRule <- function (node.attribute.name, attribute.values, node.shapes, default.shape='ELLIPSE', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!node.attribute.name %in% getNodeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setNodeShapeRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
        return ()
    }
    
    # ensure correct node shapes
    node.shapes <- toupper(node.shapes)
    unique.node.shapes <- unique(node.shapes)
    wrong.node.shape <- sapply(unique.node.shapes, function(x) !(x %in% getNodeShapes(network=network, base.url=base.url)))
    if (any(wrong.node.shape)){
        write (sprintf('ERROR in RCy3::setNodeShapeRule. You tried to use invalid node shapes. For valid ones use getNodeShapes'), stderr())
        return(NA)
    }
    
    # set default
    setDefaultNodeShape (default.shape, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(node.shapes[1]))
    
    # discrete mapping
    discreteMapping (node.attribute.name, attribute.values, node.shapes,
                     visual.property="NODE_SHAPE", columnType=columnType, style=style.name)
} # setNodeShapeRule

#------------------------------------------------------------------------------------------------------------------------
setNodeSizeRule <- function (node.attribute.name, control.points, node.sizes, mode, default.size=40, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # define the column type
    columnType <- .findColumnType(typeof(control.points[1]))
    
    # lock node dimensions
    lockNodeDimensions (TRUE)
    
    # set default
    setDefaultNodeSize (default.size, style.name)
    
    if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than control.points
        if (length (control.points) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
            node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
            write ("RCy3::setNodeSizeRule, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
        }
        
        good.args = length (control.points) == (length (node.sizes) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (node.sizes)), stderr ())
            write ("Error! RCy3:setNodeSizeRule, interpolate mode.", stderr ())
            write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
            return ()
        }
        continuousMapping (node.attribute.name, control.points, node.sizes,
                           visual.property="NODE_SIZE",
                           columnType=columnType, style=style.name)
        
    } # if mode==interpolate
    
    else { # use a discrete rule, with no interpolation
        good.args = length (control.points) == length (node.sizes)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (node.sizes)), stderr ())
            write ("Error! RCy3:setNodeSizeRule. Expecting exactly as many node.sizes as control.points in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(node.attribute.name, control.points, node.sizes,
                        visual.property="NODE_SIZE",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
    
} # setNodeSizeRule
#
#------------------------------------------------------------------------------------------------------------------------
setEdgeColorRule <- function (edge.attribute.name, control.points, colors, mode="interpolate", default.color='#FFFFFF', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
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
    columnType <- .findColumnType(typeof(control.points[1]))
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
        if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, control.points, colors,
                           visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, control.points, colors,
                        visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
} # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeOpacityRule <- function (edge.attribute.name, control.points, opacities, mode, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
        return ()
    }
    
    # set default
    setDefaultEdgeOpacity (default.opacity, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(control.points[1]))
    
    # in a previous Cytoscape version the three elements were set seperately
    #aspects = c ('Edge Opacity', 'Edge Target Arrow Opacity', 'Edge Source Arrow Opacity')
    
    if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
        if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
            opacities = c (opacities [1], opacities, opacities [length (opacities)])
            write ("RCy3::setEdgeOpacityRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (control.points) == (length (opacities) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setEdgeOpacityRule, interpolate mode.", stderr ())
            write ("Expecting 1 opacity value for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
            return ()
        }
        continuousMapping (edge.attribute.name, control.points, opacities,
                           visual.property="EDGE_TRANSPARENCY",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation
        good.args = length (control.points) == length (opacities)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (opacities)), stderr ())
            write ("Error! RCy3:setEdgeColorRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
            return ()
        }
        discreteMapping(edge.attribute.name, control.points, opacities,
                        visual.property="EDGE_TRANSPARENCY",
                        columnType=columnType, style=style.name)
    } # else: !interpolate
} # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeLineStyleRule <- function (edge.attribute.name, attribute.values, line.styles, default.style='SOLID', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeLineStyleRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # ensure correct values
    line.styles <- toupper(line.styles)
    unique.values <- unique(line.styles)
    wrong.values <- sapply(unique.values, function(x) !(x %in% getLineStyles(network=network, base.url=base.url)))
    if (any(wrong.values)){
        write (sprintf ('ERROR in RCy3::setEdgeLineStyleRule. Invalid value. For valid values use getLineStyles'), stderr ())
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
} # setEdgeLineStyleRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeLineWidthRule <- function (edge.attribute.name, attribute.values, line.widths, mode="interpolate", default.width=1, style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeLineWidthRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
        return ()
    }
    
    # unconventional arg.name
    control.points = attribute.values
    
    # set default
    default.width.list <- list(visualProperty = "EDGE_WIDTH", value = default.width)
    setVisualProperty(default.width.list, style.name)
    
    # define the column type
    columnType <- .findColumnType(typeof(line.widths[1]))
    #columnType <- 'String'
    
    if (mode=='interpolate') {  # need a 'below' width and an 'above' width  so there should be two more width than control.points
        if (length (control.points) == length (line.widths)) { # caller did not supply 'below' and 'above' values; manufacture them
            line.widths = c (line.widths [1], line.widths, line.widths [length (line.widths)])
            write ("RCy3::setEdgeLineWidthRule, no 'below' or 'above' widths specified.  Inferred from supplied widths.", stderr ());
        } 
        good.args = length (control.points) == (length (line.widths) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (line.widths)), stderr ())
            write ("Error! RCy3:setEdgeLineWidthRule, interpolate mode.", stderr ())
            write ("Expecting 1 widths for each control.point, one for 'above' widths, one for 'below' widths", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, control.points, line.widths,
                           visual.property="EDGE_WIDTH",
                           columnType=columnType, style=style.name)
    } 
    else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (line.widths)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (line.widths)), stderr ())
            write ("Error! RCy3:setEdgeLineWidthRule.  Expecting exactly as many widths as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping (edge.attribute.name, control.points, line.widths,
                         visual.property="EDGE_WIDTH", columnType=columnType, style=style.name)
        
    } 
}

#------------------------------------------------------------------------------------------------------------------------
setEdgeTargetArrowRule <- function (edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeTargetArrowRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
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
} # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeSourceArrowRule <- function (edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    id = getNetworkSuid(network)
    
    if (!edge.attribute.name %in% getEdgeAttributeNames(network=network, base.url=base.url)) {
        write (sprintf ('Warning! RCy3::setEdgeSourceArrowRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
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
} # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeTargetArrowColorRule <- function (edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeTargetArrowColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
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
    columnType <- .findColumnType(typeof(control.points[1]))
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
        if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeTargetArrowColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeTargetArrowColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, control.points, colors,
                           visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeTargetArrowColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, control.points, colors,
                        visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
} # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setEdgeSourceArrowColorRule <- function (edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default', network=NULL, base.url=.defaultBaseUrl) {
    
    if (!mode %in% c ('interpolate', 'lookup')) {
        write ("Error! RCy3:setEdgeSourceArrowColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
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
    columnType <- .findColumnType(typeof(control.points[1]))
    
    
    if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
        if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
            colors = c (colors [1], colors, colors [length (colors)])
            write ("RCy3::setEdgeSourceArrowColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
        } 
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeSourceArrowColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
        }
        
        continuousMapping (edge.attribute.name, control.points, colors,
                           visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                           columnType=columnType, style=style.name)
    } # if mode==interpolate
    else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeSourceArrowColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(edge.attribute.name, control.points, colors,
                        visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                        columnType=columnType, style=style.name)
    } # else: !interpolate, aka lookup
    
} # setEdgeSourceArrowColorRule



