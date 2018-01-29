#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R Internal.R

#--- bypasses ---------------------------------------------
setGeneric ('setNodeSizeDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeSizeDirect'))
setGeneric ('setNodeLabelDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.labels) standardGeneric ('setNodeLabelDirect'))
setGeneric ('setNodeFontSizeDirect',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeFontSizeDirect'))
setGeneric ('setNodeLabelColorDirect',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeLabelColorDirect'))
setGeneric ('setNodeWidthDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.widths) standardGeneric ('setNodeWidthDirect'))
setGeneric ('setNodeHeightDirect',        signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.heights) standardGeneric ('setNodeHeightDirect'))
setGeneric ('setNodeShapeDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.shapes) standardGeneric ('setNodeShapeDirect'))
setGeneric ('setNodeImageDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, image.positions) standardGeneric ('setNodeImageDirect'))
setGeneric ('setNodeColorDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeColorDirect'))
setGeneric ('setNodeBorderWidthDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeBorderWidthDirect'))
setGeneric ('setNodeBorderColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeBorderColorDirect'))
setGeneric ('setNodeOpacityDirect',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeOpacityDirect'))
setGeneric ('setNodeFillOpacityDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeFillOpacityDirect'))
setGeneric ('setNodeLabelOpacityDirect',  signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeLabelOpacityDirect'))
setGeneric ('setNodeBorderOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeBorderOpacityDirect'))
setGeneric ('hideNodes',                  signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('hideNodes'))
setGeneric ('unhideNodes',                signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('unhideNodes'))
setGeneric ('hideSelectedNodes',          signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('hideSelectedNodes'))

setGeneric ('setEdgeColorDirect',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeColorDirect'))
setGeneric ('setEdgeLabelDirect',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelDirect'))
setGeneric ('setEdgeFontFaceDirect',           signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeFontFaceDirect'))
setGeneric ('setEdgeFontSizeDirect',           signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeFontSizeDirect'))
setGeneric ('setEdgeLabelColorDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelColorDirect'))
setGeneric ('setEdgeTooltipDirect',            signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTooltipDirect'))
setGeneric ('setEdgeLineWidthDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLineWidthDirect'))
setGeneric ('setEdgeLineStyleDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeLineStyleDirect'))
setGeneric ('setEdgeOpacityDirect',            signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeOpacityDirect'))
setGeneric ('setEdgeSourceArrowShapeDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeSourceArrowShapeDirect'))
setGeneric ('setEdgeTargetArrowShapeDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTargetArrowShapeDirect'))
setGeneric ('setEdgeSourceArrowColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.colors) standardGeneric ('setEdgeSourceArrowColorDirect'))
setGeneric ('setEdgeTargetArrowColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.colors) standardGeneric ('setEdgeTargetArrowColorDirect'))
setGeneric ('setEdgeLabelOpacityDirect',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelOpacityDirect'))
setGeneric ('setEdgeSourceArrowOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeSourceArrowOpacityDirect'))
setGeneric ('setEdgeTargetArrowOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTargetArrowOpacityDirect'))
setGeneric ('hideSelectedEdges',               signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('hideSelectedEdges'))

setGeneric ('unhideAll', signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('unhideAll'))

# ------------------------------------------------------------------------------
setNodePropertyDirect <- function(obj=CytoscapeWindowFromNetwork(), node.names, new.values, visual.property) {
    # get network ID 
    net.SUID <- as.character(obj@suid)
    
    # cyREST allows for multiple views per network
    # get all views that associate with this network and select the first one
    net.views.SUIDs <- .getNetworkViews(obj)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    node.SUIDs <- .nodeNameToNodeSUID(obj, node.names)
    # 'node.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(node.names))
    }
    if (length(new.values) != length(node.names)) {
        write(sprintf("ERROR in setNodePropertyDirect():\n   the number of nodes [%d] and new values [%d] are not the same >> node(s) attribute couldn't be set", 
                      length(node.names), length(new.values)), stderr())
        return()
    } else if (length(node.names)==1) {
        # only one node
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", node.SUIDs, sep="/")
        node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=new.values)))
        request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
    } else {
        # multiple nodes
        for (i in seq(node.SUIDs)) {
            node.SUID <- as.character(node.SUIDs[i])
            current.value <- new.values[i]
            
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", node.SUID, sep="/")
            node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
            request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
        } # end for (node.SUID in node.SUIDs)
    }
    invisible(request.res)
}
## END setNodePropertyDirect

# ------------------------------------------------------------------------------
setEdgePropertyDirect <- function(obj=CytoscapeWindowFromNetwork(), edge.names, new.values, visual.property) {
    # get network ID 
    net.SUID <- as.character(obj@suid)
    
    
    # cyREST allows for multiple views per network
    # get all views that exist for this network and select the first one
    net.views.SUIDs <- .getNetworkViews(obj)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    edge.SUIDs <- .edgeNameToEdgeSUID(obj, edge.names)
    
    # 'edge.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(edge.names))
    }
    if (length(new.values) != length(edge.names)) {
        write(sprintf("ERROR in setEdgePropertyDirect():\n\t number of edge.names [%d] and new.values [%d] are not the same >> edge(s) attribute could not be set", 
                      length(edge.names), length(new.values)), stderr())
    } else {
        request.res <- c()
        for (i in seq(edge.SUIDs)) { 
            edge.SUID <- as.character(edge.SUIDs[i])
            current.value <- new.values[i]
            
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "edges", edge.SUID, sep="/")
            edge.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
            request.res <- PUT(url=resource.uri, body=edge.SUID.JSON, encode="json")
        }
        invisible(request.res)
    }
}
## END setEdgePropertyDirect


#========================================================================================================================
# Individual Properties
#==========================

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.colors) {
               for (current.color in new.colors){
                   # ensure the new color string is in correct hexadecimal format
                   if (.isNotHexColor(current.color)){
                       return()
                   } 
               }
               # set the node color direct
               return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_FILL_COLOR"))
           })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are unlocked (that is not tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeSizeDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.sizes) {
               # unlock node dimensions
               lockNodeDimensions (obj, FALSE)
               
               for (current.size in new.sizes){
                   # ensure the sizes are numbers
                   if (!is.double(current.size)) {
                       write (sprintf ('illegal size string "%s" in RCy3::setNodeSizeDirect. It needs to be a number.', current.size), stderr ())
                       return ()
                   }
               }
               # set the node properties direct
               setNodePropertyDirect(obj, node.names, new.sizes, "NODE_WIDTH")
               setNodePropertyDirect(obj, node.names, new.sizes, "NODE_HEIGHT")
           })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is not tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeWidthDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.widths) {
               # unlock node dimensions
               lockNodeDimensions (obj, FALSE)
               
               for (current.width in new.widths){
                   # ensure the width(s) are numbers
                   if (!is.double(current.width)) {
                       write (sprintf ('illegal node width "%s" in RCy3::setNodeWidthDirect. Width needs to be a number.', current.width), stderr ())
                       return ()
                   }
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.widths, "NODE_WIDTH"))
           })

#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeHeightDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.heights) { # Comment Tanja: Could pass in visual style here
               # unlock node dimensions
               lockNodeDimensions (obj, FALSE)
               
               for (current.height in new.heights){
                   # ensure the height(s) are numbers
                   if (!is.double(current.height)) {
                       write (sprintf ('illegal height string "%s" in RCy3::setNodeHeightDirect. It needs to be a number.', current.height), stderr ())
                       return ()
                   }
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.heights, "NODE_HEIGHT"))
           })

# ------------------------------------------------------------------------------
setMethod('setNodeLabelDirect', 'OptionalCyWinClass', 
          function(obj, node.names, new.labels) {
              setNodePropertyDirect(obj, node.names, new.labels, "NODE_LABEL")
          })
## END setNodeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setNodeFontSizeDirect', 'OptionalCyWinClass', 
          function(obj, node.names, new.sizes) {
              size.type.errors = 0
              
              for(current.size in new.sizes) {
                  if(!is.double(current.size)) {
                      write(sprintf("ERROR in RCy3::setNodeFontSizeDirect():\n\t font size '%s' has to be numerical value", current.size), stderr())
                      
                      size.type.errors = size.type.errors + 1
                  }
              }
              
              if(size.type.errors < 1) {
                  setNodePropertyDirect(obj, node.names, new.sizes, "NODE_LABEL_FONT_SIZE")
              }
          })
## END setNodeFontSizeDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelColorDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.colors) {
               for (current.color in new.colors){
                   # ensure the color is formated in the correct hexadecimal style
                   if (.isNotHexColor(current.color)){
                       return()
                   } 
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_LABEL_COLOR"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.shapes) {
               if (length (node.names) != length (new.shapes)) {
                   if (length(new.shapes) != 1){
                       msg = sprintf ('error in RCy3::setNodeShapeDirect.  new.shapes count (%d) is neither 1 nor same as node.names count (%d)',
                                      length (new.shapes), length (node.names))
                       write (msg, stderr ())
                       return ()
                   }
               }
               
               # convert old to new node shapes
               new.shapes[new.shapes=='round_rect'] <- 'ROUND_RECTANGLE'
               new.shapes[new.shapes=='rect'] <- 'RECTANGLE'
               
               # ensure correct node shapes
               new.shapes <- toupper(new.shapes)
               unique.node.shapes <- unique(new.shapes)
               wrong.node.shape <- sapply(unique.node.shapes, function(x) !(x %in% getNodeShapes(obj)))
               if (any(wrong.node.shape)){
                   write (sprintf ('ERROR in RCy3::setNodeShapeDirect. %s is not a valid shape. Please note that some older shapes are no longer available. For valid ones check getNodeShapes.', new.shapes), stderr ())
                   return(NA)
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.shapes, "NODE_SHAPE"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeImageDirect', 'OptionalCyWinClass',
           
           function (obj, node.names, image.positions) {
               
               write(sprintf("WARNING: Method RCy3::setNodeImageDirect() is not implemented in RCy3!"), stderr())
               
               return(FALSE)
               
               #THIS WILL NOT BE EXECUTED
               # insert a warning 
               if (!is.numeric(image.positions)){
                   msg = sprintf ('Error in RCy3::setNodeImageDirect. Note that image urls are no longer supported. Upload your image into the Image Manager in the style tab in the control panel and report its position in the Image Manager as number.')
                   write (msg, stderr ())
                   return()
               }
               
               if (length (node.names) != length (image.positions)) {
                   if (length (image.positions) == 1){
                       msg = sprintf ('Error in RCy3::setNodeImageDirect. image.positions count (%d) is neither 1 nor same as node.names count (%d)',
                                      length (image.positions), length (node.names))
                       write (msg, stderr ())
                       return ()
                   }
               }
               # only allow for upto 9 custom graphics
               if ((length(unique(image.positions)))>9){
                   msg = sprintf ('Error in RCy3::setNodeImageDirect. Cytoscape only supports upto 9 custom graphics.')
                   write (msg, stderr ())
                   return()
               }
               
               # TODO check if enough open spaces
               # get node images from properties
               
               # pseudo code:
               # for loop
               # if is == "org.cytoscape.ding.customgraphics.NullCustomGraphics,0,[ Remove Graphics ],"
               # if "NODE_CUSTOMGRAPHICS_9" passed and still not: error message
               # not working: return(setNodePropertyDirect(obj, node.names, image.urls, "NODE_CUSTOMGRAPHICS_1"))
               return(setNodePropertyDirect(obj, node.names, paste0("org.cytoscape.ding.customgraphics.bitmap.URLImageCustomGraphics,", image.positions, ",bundle"), "NODE_CUSTOMGRAPHICS_1"))
               
               # test code to remove an image
               # setNodePropertyDirect(obj, node.names, "org.cytoscape.ding.customgraphics.NullCustomGraphics,0,[ Remove Graphics ],", "NODE_CUSTOMGRAPHICS_1")
               
               #         
               #         # the below code is from a previous RCytoscape version
               #         for (i in 1:length (node.names)) {
               #             setNodeShapeDirect (obj, node.names [i], 'rect')
               #             setNodeLabelDirect (obj, node.names [i], '')
               #             result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names [i], 'Node Custom Graphics 1', image.urls [i])
               #         }
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.sizes) {
               for (current.size in new.sizes){
                   # ensure the widths are numbers
                   if (!is.double(current.size)) {
                       write (sprintf ('illegal width string "%s" in RCy3::setNodeBorderWidthDirect. It needs to be a number.', current.size), stderr ())
                       return ()
                   }
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.sizes, "NODE_BORDER_WIDTH"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.colors) {
               # ensure the color is formated in correct hexadecimal style
               for (color in new.colors){
                   if (.isNotHexColor(color)){
                       return()
                   } 
               }
               # set the node border color direct
               return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_BORDER_PAINT"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeOpacityDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.values) {
               for (current.value in new.values){
                   # ensure the opacity value is a double and between 0 and 255
                   if (! is.double(current.value) || current.value < 0  || current.value > 255) {
                       write (sprintf ('RCy3::setNodeOpacityDirect: illegal opacity string "%s". It needs to be between 0 and 255.', current.value), stderr ())
                       return ()
                   }
               }
               setNodePropertyDirect(obj, node.names, new.values, "NODE_TRANSPARENCY")
               setNodePropertyDirect(obj, node.names, new.values, "NODE_BORDER_TRANSPARENCY")
               setNodePropertyDirect(obj, node.names, new.values, "NODE_LABEL_TRANSPARENCY")
           })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFillOpacityDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.values) {
               for (current.value in new.values){
                   # ensure the opacity value is between 0 and 255
                   if (!is.double(current.value) || current.value < 0  || current.value > 255) {
                       write (sprintf ('illegal opacity string "%s" in RCy3::setNodeFillOpacityDirect. It needs to be a double and between 0 and 255.', current.value), stderr ())
                       return ()
                   }
               }
               # set the node border color direct
               return(setNodePropertyDirect(obj, node.names, new.values, "NODE_TRANSPARENCY"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderOpacityDirect', 'OptionalCyWinClass',
           function (obj, node.names, new.values) {
               for (current.value in new.values){
                   # ensure the opacity value is a double and between 0 and 255
                   if (! is.double(current.value) || current.value < 0  || current.value > 255) {
                       write (sprintf ('illegal opacity string "%s" in RCy3::setNodeBorderOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
                       return ()
                   }
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.values, "NODE_BORDER_TRANSPARENCY"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelOpacityDirect', 'OptionalCyWinClass',
           
           function (obj, node.names, new.values) {
               for (current.value in new.values){
                   # ensure the opacity value is a double and between 0 and 255
                   if (! is.double(current.value) || current.value < 0  || current.value > 255) {
                       write (sprintf ('illegal opacity string "%s" in RCy3::setNodeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
                       return ()
                   }
               }
               # set the node property direct
               return(setNodePropertyDirect(obj, node.names, new.values, "NODE_LABEL_TRANSPARENCY"))
           })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'OptionalCyWinClass',
           
           function (obj) {
               node.names <- getSelectedNodes(obj)
               setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
           }) # hideSelectedNodes

# ------------------------------------------------------------------------------
setMethod('hideNodes', 'OptionalCyWinClass', 
          function(obj, node.names) {
              setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
          }) 
## END hideNodes

# ------------------------------------------------------------------------------------------------------------------------
setMethod('unhideNodes', 'OptionalCyWinClass', 
          function(obj, node.names) {
              setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
          }) 
## END unhideNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityDirect', 'OptionalCyWinClass',
           
           function (obj, edge.names, new.values) {
               for (current.value in new.values){
                   # ensure the opacity value is a double and between 0 and 255
                   if (! is.double(current.value) || current.value < 0  || current.value > 255) {
                       write (sprintf ('illegal opacity string "%s" in RCy3::setEdgeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
                       return ()
                   }
               }
               # set the edge property direct
               #     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
               setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_LABEL_TRANSPARENCY")
               setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TRANSPARENCY")
           })

# ------------------------------------------------------------------------------
setMethod('setEdgeColorDirect', 'OptionalCyWinClass',
          function (obj, edge.names, new.value) {
              # ensure the color is formated in correct hexadecimal style
              for (color in new.value){
                  if (.isNotHexColor(color)){
                      return()
                  } 
              }
              # set the edge color direct
              # TODO maybe this should be EDGE_PAINT
              return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_STROKE_UNSELECTED_PAINT"))
          })

# ------------------------------------------------------------------------------
setMethod('setEdgeLabelDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.value) {
              setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL")
          })
## END setEdgeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontFaceDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.value) {
              setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_FACE")
          })
## END setEdgeFontFaceDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontSizeDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.value) {
              size.type.errors = 0
              
              for(current.size in new.value) {
                  # ensure the sizes are valid numbers
                  if(!is.numeric(current.size)) {
                      write(sprintf ('illegal font string "%s" in RCy3::setEdgeFontSizeDirect():\t\n it needs to be a valid number.', current.size), stderr ())
                      
                      size.type.errors = size.type.errors + 1
                  }
              }
              
              if(size.type.errors < 1) {
                  setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_SIZE")
              }
          })
## END setEdgeFontSizeDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelColorDirect', 'OptionalCyWinClass',
           function (obj, edge.names, new.value) {
               for (current.color in new.value){
                   # ensure the color is formated in correct hexadecimal style
                   if (.isNotHexColor(current.color)){
                       return()
                   }
               }
               # set the edge property direct
               return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_COLOR"))
           })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipDirect', 'OptionalCyWinClass',
           function (obj, edge.names, new.values) {
               if (length (edge.names) != length (new.values)) {
                   if (length(new.values) != 1){
                       msg = sprintf ('error in RCy3::setEdgeTooltipDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                                      length (new.values), length (edge.names))
                       write (msg, stderr ())
                       return ()
                   }
               }
               # set the edge property direct
               return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TOOLTIP"))
           })

# ------------------------------------------------------------------------------
setMethod('setEdgeLineWidthDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.value) {
              for (current.size in new.value){
                  # ensure the sizes are numbers
                  if (!is.numeric(current.size)) {
                      write (sprintf ('illegal size string "%s" in RCy3::setEdgeLineWidthDirect. It needs to be a number.', current.size), stderr ())
                      return ()
                  }
              }
              
              # set the edge property direct
              return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_WIDTH"))
          })

# ------------------------------------------------------------------------------
setMethod('setEdgeLineStyleDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.values) {
              unique.new.values <- unique(new.values)
              
              wrong.values <- 
                  sapply(unique.new.values, function(v) { !(toupper(v) %in% getLineStyles(obj)) })
              
              if(any(wrong.values)) {
                  error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
                  
                  error.msg <- paste("\n\t\tERROR in setEdgeLineStyleDirect() >> INVALID line style value(s): ", error.msg, "\n", sep="")
                  
                  write(error.msg, stderr())
                  return(FALSE)
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_LINE_TYPE"))
          })
## END setEdgeLineStyleDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowShapeDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.values) {
              unique.new.values <- unique(new.values)
              
              wrong.values <- sapply(unique.new.values, function(v) { !(toupper(v) %in% getArrowShapes(obj)) })
              
              if(any(wrong.values)) {
                  error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
                  
                  error.msg <- paste("\n\t\tERROR in setEdgeSourceArrowShapeDirect() >> INVALID arrow shape value(s): ", error.msg, "\n", sep="")
                  
                  write(error.msg, stderr())
                  return(FALSE)
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_SOURCE_ARROW_SHAPE"))
          })
## END setEdgeSourceArrowShapeDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowShapeDirect', 'OptionalCyWinClass', 
          function (obj, edge.names, new.values) {
              unique.new.values <- unique(new.values)
              
              wrong.values <- sapply(unique.new.values, function(v) { !(toupper(v) %in% getArrowShapes(obj)) })
              
              if(any(wrong.values)) {
                  error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
                  
                  error.msg <- paste("\n\t\tERROR in setEdgeTargetArrowShapeDirect() >> INVALID arrow shape value(s): ", error.msg, "\n", sep="")
                  
                  write(error.msg, stderr())
                  return(FALSE)
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_TARGET_ARROW_SHAPE"))
          })
## END setEdgeTargetArrowShapeDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowColorDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.colors) {
              for(current.color in new.colors) {
                  # check the color is represented in hexadecimal format
                  if (.isNotHexColor(current.color)){
                      return()
                  }
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_SOURCE_ARROW_UNSELECTED_PAINT"))
          })
## END setEdgeSourceArrowColorDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowColorDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.colors) {
              for(current.color in new.colors) {
                  if (.isNotHexColor(current.color)){
                      return()
                  }
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_TARGET_ARROW_UNSELECTED_PAINT"))
          })
## END setEdgeTargetArrowColorDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeLabelOpacityDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.value) {
              for(current.value in new.value) {
                  # check that the opacity value is DOUBLE number between 0 and 255
                  if(!is.double(current.value) || current.value < 0  || current.value > 255) {
                      write(sprintf("\n\t\tERROR in setEdgeLabelOpacityDirect(): illegal opacity value '%s'. Opacity needs to be number between 0 and 255", current.value), stderr())
                      
                      return(FALSE)
                  }
              }
              # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
              return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_TRANSPARENCY"))
          })
## END setEdgeLabelOpacityDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowOpacityDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.values) {
              write(sprintf("WARNING: Method RCy3::setEdgeSourceArrowOpacityDirect() is not implemented in RCy3!"), stderr())
              
              return(FALSE)
          })
## END setEdgeSourceArrowOpacityDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowOpacityDirect', 'OptionalCyWinClass', 
          function(obj, edge.names, new.values) {
              write(sprintf("WARNING: Method RCy3::setEdgeTargetArrowOpacityDirect() is not implemented in RCy3!"), stderr())
              
              return(FALSE)
          })
## END setEdgeTargetArrowOpacityDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'OptionalCyWinClass',
           
           function (obj) {
               edge.names <- getSelectedEdges(obj)
               setEdgePropertyDirect(obj, edge.names, 'false', "EDGE_VISIBLE")
           }) # hideSelectedEdges

# ------------------------------------------------------------------------------
setMethod('unhideAll', 'OptionalCyWinClass', 
          function(obj) {
              node.names <- getAllNodes(obj)
              setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
              
              edge.names <- getAllEdges(obj)
              setEdgePropertyDirect(obj, edge.names, 'true', "EDGE_VISIBLE")
          }) 
## END unhideAll


