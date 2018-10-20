# ==============================================================================
# Functions for getting and setting DEFAULT values for visual properties,
# organized into sections:
#
# I. General functions for setting node, edge and network defaults
# II. Specific functions for setting particular node, edge and network defaults
#
# ==============================================================================
# I. General Functions
# ------------------------------------------------------------------------------
#' Updates the default values of visual properties in a style
#'
#' @description Updates visual property defaults, overriding any prior settings. See mapVisualProperty for
#' the list of visual properties.
#' @param style.name (char) name for style
#' @param defaults (list) a list of visual property default settings
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDefaults('myStyle',list('node fill color'='#0000FF','node size'=50))
#' }
#' @export
#' @seealso mapVisualProperty
updateStyleDefaults <- function(style.name,defaults,base.url=.defaultBaseUrl){
    
    def.list <- list()
    for (i in seq_len(length(defaults))) {
        visual.prop.name <- names(defaults)[i]
        visual.prop.name = toupper(gsub("\\s+","_",visual.prop.name))
        visual.prop.name = switch(visual.prop.name,
                                  'EDGE_COLOR'='EDGE_UNSELECTED_PAINT',
                                  'EDGE_THICKNESS'='EDGE_WIDTH',
                                  'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                                  visual.prop.name)
        def.list[[i]] <- list(visualProperty=visual.prop.name,
                              value=defaults[[i]])
    }
    invisible(cyrestPUT(paste('styles', style.name,'defaults', sep = '/'),
                        body=def.list, base.url = base.url))
}

# ------------------------------------------------------------------------------
#' @title Get Visual Property Default
#'
#' @description Retrieve the default value for a visual property.
#' @param property Name of property, e.g., NODE_FILL_COLOR (see \link{getVisualPropertyNames})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getVisualPropertyDefault('NODE_SIZE')
#' }
#' @export
getVisualPropertyDefault <- function(property, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestGET(paste("styles", as.character(style.name), "defaults", property, sep="/"), base.url=base.url)
    return(res[[2]])
}

# ------------------------------------------------------------------------------
#' @title Set Visual Property Default
#'
#' @description Set the default value for a visual property.
#' @param style.string A named list including "visualProperty" and "value"
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setVisualPropertyDefault(list(visualProperty = "NODE_SIZE", value = 35))
#' }
#' @export
setVisualPropertyDefault <- function(style.string, style.name='default', base.url=.defaultBaseUrl) {
    res <- cyrestPUT(paste("styles", as.character(style.name), "defaults", sep="/"),
                     body = list(style.string),
                     base.url=base.url)
    invisible(res)
}

# ==============================================================================
# II. Specific Functions
# ==============================================================================
# II.a. Node Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Set Node Shape Default
#'
#' @description Set the default node shape.
#' @param new.shape Name of shape, e.g., ELLIPSE, RECTANGLE, etc (see \link{getNodeShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeShapeDefault('ELLIPSE')
#' }
#' @export
setNodeShapeDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    new.shape <- toupper(new.shape)
    if (new.shape %in% getNodeShapes(base.url)){
        style = list(visualProperty = "NODE_SHAPE", value = new.shape)
        setVisualPropertyDefault(style, style.name, base.url)
    }else{
        write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
    }
}

# ------------------------------------------------------------------------------
#' @title Set Node Size Default
#'
#' @description Set the default node size.
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeSizeDefault(35)
#' }
#' @export
setNodeSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    lockNodeDimensions(TRUE, style.name, base.url)
    
    style <- list(visualProperty = "NODE_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Color Default
#'
#' @description Set the default node color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeColorDefault('#FD5903')
#' }
#' @export
setNodeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Border Color Default
#'
#' @description Set the default node border color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderColorDefault('#FD5903')
#' }
#' @export
setNodeBorderColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Border Width Default
#'
#' @description Set the default node border width.
#' @param new.width Numeric value for width
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeBorderWidthDefault(2)
#' }
#' @export
setNodeBorderWidthDefault <-  function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Font Size Default
#'
#' @description Set the default node font size.
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeFontSizeDefault(12)
#' }
#' @export
setNodeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Label Color Default
#'
#' @description Set the default node label color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeLabelColorDefault('#FD5903')
#' }
#' @export
setNodeLabelColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Node Selection Color Default
#'
#' @description Retrieve the default selection node color.
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getNodeSelectionColorDefault()
#' }
#' @export
getNodeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NODE_SELECTED_PAINT', style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Node Selection Color Default
#'
#' @description Set the default selection node color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeSelectionColorDefault('#FD5903')
#' }
#' @export
setNodeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Bar Chart
#'
#' @description Makes a bar chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed, in order.
#' @param type Type of bar chart: GROUPED (default), STACKED, HEAT_STRIPS, or UP_DOWN
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param range (optional) Min and max values of chart. Default is to use min
#' and max from specified data columns.
#' @param orientation (optional) HORIZONTAL or VERTICAL (default).
#' @param colAxis (optional) Show axis with column labels. Default is FALSE.
#' @param rangeAxis (optional) Show axis with range of values. Default is FALSE.
#' @param zeroLine (optional) Show a line at zero. Default is FALSE.
#' @param axisWidth (optional) Width of axis lines, if shown. Default is 0.25.
#' @param axisColor (optional) Color of axis lines, if shown. Default is black.
#' @param axisFontSize (optional) Font size of axis labels, if shown. Default 
#' is 1.
#' @param separation (optional) Distance between bars. Default is 0.0.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomBarChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics
#' @export
#' @importFrom RJSONIO toJSON 
#' @importFrom stats na.omit
setNodeCustomBarChart<-function(columns, type="GROUPED", colors=NULL, 
                                range=NULL, orientation="VERTICAL", 
                                colAxis=FALSE, rangeAxis=FALSE, zeroLine=FALSE,
                                axisWidth=0.25, axisColor = "#000000",
                                axisFontSize=1, separation=0.0,
                                slot=1, style.name='default', 
                                base.url=.defaultBaseUrl){
    
    if (!type %in% c('GROUPED','STACKED','HEAT_STRIPS','UP_DOWN'))
        stop ('type must be one of the following: GROUPED, STACKED, HEAT_STRIPS, or UP_DOWN')
    
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = columns,
                  cy_type = type,
                  cy_orientation = orientation,
                  cy_showDomainAxis = colAxis,
                  cy_showRangeAxis = rangeAxis,
                  cy_showRangeZeroBaseline = zeroLine,
                  cy_axisWidth = axisWidth,
                  cy_axisColor = axisColor,
                  cy_axisLabelFontSize = axisFontSize,
                  cy_separation = separation)
    
    if (is.null(colors)){
        if (type %in% c("GROUPED","STACKED"))
            colors<-rep(.cyPalette('set1'),length.out=length(columns))
        else if (type == "HEAT_STRIPS")
            colors<-.cyPalette('rdbu')[c(2,6,10)]
        else 
            colors<-.cyPalette('rdbu')[c(2,10)]
    }
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    if (!is.null(range))
        chart[['cy_range']] <- range
    else{
        cols<-getTableColumns(columns=columns, base.url = base.url)
        min<-min(apply(na.omit(cols),2,min))
        max<-max(apply(na.omit(cols),2,max))
        chart[['cy_range']] <- c(min,max)
    }
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.BarChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Box Chart
#'
#' @description Makes a box chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed.
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param range (optional) Min and max values of chart. Default is to use min
#' and max from specified data columns.
#' @param orientation (optional) HORIZONTAL or VERTICAL (default).
#' @param rangeAxis (optional) Show axis with range of values. Default is FALSE.
#' @param zeroLine (optional) Show a line at zero. Default is FALSE.
#' @param axisWidth (optional) Width of axis lines, if shown. Default is 0.25.
#' @param axisColor (optional) Color of axis lines, if shown. Default is black.
#' @param axisFontSize (optional) Font size of axis labels, if shown. Default 
#' is 1.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomBoxChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics
#' @export
#' @importFrom RJSONIO toJSON  
#' @importFrom stats na.omit
setNodeCustomBoxChart<-function(columns, colors=NULL, 
                                range=NULL, orientation="VERTICAL", 
                                rangeAxis=FALSE, zeroLine=FALSE,
                                axisWidth=0.25, axisColor = "#000000",
                                axisFontSize=1, 
                                slot=1, style.name='default', 
                                base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = columns,
                  cy_orientation = orientation,
                  cy_showRangeAxis = rangeAxis,
                  cy_showRangeZeroBaseline = zeroLine,
                  cy_axisWidth = axisWidth,
                  cy_axisColor = axisColor,
                  cy_axisLabelFontSize = axisFontSize)
    
    if (is.null(colors))
        colors<-rep(.cyPalette('set1'),length.out=length(columns))
    
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    if (!is.null(range))
        chart[['cy_range']] <- range
    else{
        cols<-getTableColumns(columns=columns, base.url = base.url)
        min<-min(apply(na.omit(cols),2,min))
        max<-max(apply(na.omit(cols),2,max))
        chart[['cy_range']] <- c(min,max)
    }
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.BoxChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom HeatMap Chart
#'
#' @description Makes a heatmap chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed.
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param range (optional) Min and max values of chart. Default is to use min
#' and max from specified data columns.
#' @param orientation (optional) VERTICAL or HORIZONTAL (default).
#' @param rangeAxis (optional) Show axis with range of values. Default is FALSE.
#' @param zeroLine (optional) Show a line at zero. Default is FALSE.
#' @param axisWidth (optional) Width of axis lines, if shown. Default is 0.25.
#' @param axisColor (optional) Color of axis lines, if shown. Default is black.
#' @param axisFontSize (optional) Font size of axis labels, if shown. Default 
#' is 1.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomHeatMapChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics
#' @export
#' @importFrom RJSONIO toJSON 
#' @importFrom stats na.omit 
setNodeCustomHeatMapChart<-function(columns, colors=NULL, 
                                    range=NULL, orientation="HORIZONTAL", 
                                    rangeAxis=FALSE, zeroLine=FALSE,
                                    axisWidth=0.25, axisColor = "#000000",
                                    axisFontSize=1, 
                                    slot=1, style.name='default', 
                                    base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = rev(columns), #rev for left-to-right ordering
                  cy_orientation = orientation,
                  cy_showRangeAxis = rangeAxis,
                  cy_showRangeZeroBaseline = zeroLine,
                  cy_axisWidth = axisWidth,
                  cy_axisColor = axisColor,
                  cy_axisLabelFontSize = axisFontSize)
    
    if (is.null(colors))
        colors<-c(.cyPalette('rdbu')[c(2,6,10)],"#888888")
    
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    if (!is.null(range))
        chart[['cy_range']] <- range
    else{
        cols<-getTableColumns(columns=columns, base.url = base.url)
        min<-min(apply(na.omit(cols),2,min))
        max<-max(apply(na.omit(cols),2,max))
        chart[['cy_range']] <- c(min,max)
    }
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.HeatMapChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Line Chart
#'
#' @description Makes a line chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed.
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param range (optional) Min and max values of chart. Default is to use min
#' and max from specified data columns.
#' @param lineWidth (optional) Width of chart line. Default is 1.0.
#' @param rangeAxis (optional) Show axis with range of values. Default is FALSE.
#' @param zeroLine (optional) Show a line at zero. Default is FALSE.
#' @param axisWidth (optional) Width of axis lines, if shown. Default is 0.25.
#' @param axisColor (optional) Color of axis lines, if shown. Default is black.
#' @param axisFontSize (optional) Font size of axis labels, if shown. Default 
#' is 1.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomLineChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics
#' @export
#' @importFrom RJSONIO toJSON  
#' @importFrom stats na.omit
setNodeCustomLineChart<-function(columns, colors=NULL, 
                                 range=NULL, lineWidth=1.0, 
                                 rangeAxis=FALSE, zeroLine=FALSE,
                                 axisWidth=0.25, axisColor = "#000000",
                                 axisFontSize=1, 
                                 slot=1, style.name='default', 
                                 base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = columns,
                  cy_lineWidth = lineWidth,
                  cy_showRangeAxis = rangeAxis,
                  cy_showRangeZeroBaseline = zeroLine,
                  cy_axisWidth = axisWidth,
                  cy_axisColor = axisColor,
                  cy_axisLabelFontSize = axisFontSize)
    
    if (is.null(colors))
        colors<-rep(.cyPalette('set1'),length.out=length(columns))
    
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    if (!is.null(range))
        chart[['cy_range']] <- range
    else{
        cols<-getTableColumns(columns=columns, base.url = base.url)
        min<-min(apply(na.omit(cols),2,min))
        max<-max(apply(na.omit(cols),2,max))
        chart[['cy_range']] <- c(min,max)
    }
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.LineChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Pie Chart
#'
#' @description Makes a pie chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed.
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param startAngle (optional) Angle to start filling pie. Default is 0.0.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomPieChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics
#' @export
#' @importFrom RJSONIO toJSON 
setNodeCustomPieChart<-function(columns, colors=NULL, 
                                 startAngle=0.0, 
                                 slot=1, style.name='default', 
                                 base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = columns,
                  cy_startAngle = startAngle)
    
    if (is.null(colors))
        colors<-rep(.cyPalette('set1'),length.out=length(columns))
    
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.PieChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Ring Chart
#'
#' @description Makes a ring chart per node using specified node table columns by
#' setting a default custom graphic style.
#' @param columns List of node column names to be displayed.
#' @param colors (optional) List of colors to be matched with columns or with
#' range, depending on type. Default is a set of colors from an appropriate 
#' Brewer palette.
#' @param startAngle (optional) Angle to start filling ring Default is 0.0.
#' @param holeSize (optional) Size of hole in ring. Ranges 0-1. Default is 0.5.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomRingChart(c("data1","data2","data3"))
#' }
#' @seealso setNodeCustomPosition, removeNodeCustomGraphics 
#' @export
#' @importFrom RJSONIO toJSON 
setNodeCustomRingChart<-function(columns, colors=NULL, 
                                startAngle=0.0, holeSize = 0.5,
                                slot=1, style.name='default', 
                                base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_dataColumns = columns,
                  cy_startAngle = startAngle,
                  cy_holeSize = holeSize)
    
    if (is.null(colors))
        colors<-rep(.cyPalette('set1'),length.out=length(columns))
    
    chart[['cy_colors']] <- colors
    chart[['cy_colorScheme']] <- "Custom"
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.RingChart",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Linear Gradient
#'
#' @description Makes a gradient fill per node by setting a default custom 
#' graphic style.
#' @param colors (optional) List of colors to define gradient
#' @param anchors (optional) Position of colors from 0.0 to 1.0.
#' @param angle (optional) Angle of gradient. Default is 0 (left-to-right).
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomLinearGradient()
#' }
#' @export
setNodeCustomLinearGradient<-function(colors=c("#DDDDDD","#888888"), anchors=c(0.0,1.0), angle=0.0, 
                                      slot=1, style.name='default', 
                                      base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_angle = angle,
                  cy_gradientColors = colors,
                  cy_gradientFractions = anchors)
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.LinearGradient",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name)
}

# ------------------------------------------------------------------------------
#' @title Set Node Custom Radial Gradient
#'
#' @description Makes a gradient fill per node by setting a default custom 
#' graphic style.
#' @param colors (optional) List of colors to define gradient
#' @param anchors (optional) Position of colors from 0.0 to 1.0.
#' @param xCenter (optional) X position for center of radial effect from 0.0 
#' to 1.0. Default is 0.5.
#' @param yCenter (optional) Y position for center of radial effect from 0.0 
#' to 1.0. Default is 0.5.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomRadialGradient()
#' }
#' @export
setNodeCustomRadialGradient<-function(colors=c("#DDDDDD","#888888"), anchors=c(0.0,1.0), 
                                      xCenter=0.5, yCenter=0.5, 
                                      slot=1, style.name='default', 
                                      base.url=.defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    chart <- list(cy_gradientColors = colors,
                  cy_gradientFractions = anchors,
                  cy_center = list(x = xCenter,
                                   y = yCenter))
    
    style.string = list(visualProperty = vp, value = paste("org.cytoscape.RadialGradient",toJSON(chart),sep = ":"))
    setVisualPropertyDefault(style.string, style.name)
}
# ------------------------------------------------------------------------------
#' @title Set Node Custom Position
#'
#' @description Adjust the position of a custom graphic relative to its node.
#' @param nodeAnchor Position on node to place the graphic: NW,N,NE,E,SE,S,SW,W 
#' or C for center (default)
#' @param graphicAnchor Position on graphic to place on node: NW,N,NE,E,SE,S,SW,W 
#' or C for center (default)
#' @param justification Positioning of content within graphic: l,r,c (default)
#' @param xOffset Additional offset in the x direction
#' @param yOffset Additional offset in the y direction
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setNodeCustomPosition()
#' }
#' @export
setNodeCustomPosition<-function(nodeAnchor="C", graphicAnchor="C", justification="c", 
                                xOffset=0.0, yOffset=0.0, slot=1, style.name='default',
                                base.url = .defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS_POSITION',as.character(slot),sep='_')
    
    
    style.string = list(visualProperty = vp, value = paste(nodeAnchor,graphicAnchor,
                                                           justification,xOffset,yOffset,
                                                           sep = ","))
    setVisualPropertyDefault(style.string, style.name, base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Remove Node Custom Graphics
#'
#' @description Remove the default custom charts, images and gradients.
#' @param slot (optional) Which custom graphics slot to modify. Slots 1-9 are 
#' available for independent charts, gradients and images. Default is 1.
#' @param style.name (optional) Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' removeNodeCustomGraphics()
#' }
#' @export
removeNodeCustomGraphics<-function(slot=1, style.name='default',
                                base.url = .defaultBaseUrl){
    if (!slot %in% seq_len(9))
        stop ('slot must be an integer between 1 and 9')
    vp<-paste('NODE_CUSTOMGRAPHICS',as.character(slot),sep='_')
    
    setVisualPropertyDefault(list(visualProperty = vp, value = NULL), 
                             style.name, base.url = base.url)
}

# ==============================================================================
# II.b. Edge Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Set Edge Line Width Default
#'
#' @description Set the default edge width.
#' @param new.width Numeric value for width
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineWidthDefault(3)
#' }
#' @export
setEdgeLineWidthDefault <- function(new.width, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Line Style Default
#'
#' @description Set the default edge style.
#' @param new.line.style  Name of line style, e.g., SOLID, LONG_DASH, etc (see \link{getLineStyles})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeLineStyleDefault('LONG_DASH')
#' }
#' @export
setEdgeLineStyleDefault <- function(new.line.style, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Color Default
#'
#' @description Set the default edge color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeColorDefault('#FD5903')
#' }
#' @export
setEdgeColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Color Default
#'
#' @description Set the default edge source arrow color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowColorDefault('#FD5903')
#' }
#' @export
setEdgeSourceArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Color Default
#'
#' @description Set the default edge target arrow color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowColorDefault('#FD5903')
#' }
#' @export
setEdgeTargetArrowColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Source Arrow Shape Default
#'
#' @description Set the default edge source arrow shape.
#' @param new.shape Name of shape, e.g., ARROW, T, etc (see \link{getArrowShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSourceArrowShapeDefault('ARROW')
#' }
#' @export
setEdgeSourceArrowShapeDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_SOURCE_ARROW_SHAPE", value = new.shape) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Target Arrow Shape Default
#'
#' @description Set the default edge target arrow shape.
#' @param new.shape Name of shape, e.g., ARROW, T, etc (see \link{getArrowShapes})
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeTargetArrowShapeDefault('ARROW')
#' }
#' @export
setEdgeTargetArrowShapeDefault <- function(new.shape, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_TARGET_ARROW_SHAPE", value = new.shape) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Set Edge Font Size Default
#'
#' @description Set the default edge font size.
#' @param new.size Numeric value for size
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeFontSizeDefault(12)
#' }
#' @export
setEdgeFontSizeDefault <- function(new.size, style.name='default', base.url=.defaultBaseUrl) {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setVisualPropertyDefault(style, style.name, base.url)
}

# ------------------------------------------------------------------------------
#' @title Get Edge Selection Color Default
#'
#' @description Retrieve the default selected edge color.
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getEdgeSelectionColorDefault()
#' }
#' @export
getEdgeSelectionColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    matched <- unname(getStyleDependencies()['arrowColorMatchesEdge'])
    if(matched)
        return(getVisualPropertyDefault('EDGE_SELECTED_PAINT',style.name, base.url))
    else
        return(getVisualPropertyDefault('EDGE_STROKE_SELECTED_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Edge Selection Color Default
#'
#' @description Set the default selected edge color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setEdgeSelectionColorDefault('#FD5903')
#' }
#' @export
setEdgeSelectionColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "EDGE_SELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
    style = list(visualProperty = "EDGE_STROKE_ELECTED_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

# ==============================================================================
# II.c. Network Properties
# Pattern A: (1) prepare input value as named list, (2) call setVisualPropertyDefault()
# Pattern B: (1) call getVisualPropertyDefault()
# ------------------------------------------------------------------------------
#' @title Get Background Color Default
#'
#' @description Retrieve the default background color.
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' getBackgroundColorDefault()
#' }
#' @export
getBackgroundColorDefault <- function(style.name='default', base.url=.defaultBaseUrl) {
    return(getVisualPropertyDefault('NETWORK_BACKGROUND_PAINT',style.name, base.url))
}

# ------------------------------------------------------------------------------
#' @title Set Background Color Default
#'
#' @description Set the default background color.
#' @param new.color Color as hex code, e.g., #FD5903
#' @param style.name Name of style; default is "default" style
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' setBackgroundColorDefault('#888888')
#' }
#' @export
setBackgroundColorDefault <- function(new.color, style.name='default', base.url=.defaultBaseUrl) {
    if (.isNotHexColor(new.color)){
        return()
    } 
    style = list(visualProperty = "NETWORK_BACKGROUND_PAINT", value = new.color) 
    setVisualPropertyDefault(style, style.name, base.url)
}

