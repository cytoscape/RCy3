# ==============================================================================
# Functions for working with ANNOTATIONS for the addition and modification of 
# graphical annotation objects. In the Cytoscape user interface, annotations are
# managed in the Annotation tab of the Control Panel.
#
# ==============================================================================

# TODO: list of available commands for annotations
#commandsHelp("annotation")
#commandsHelp("annotation add text")

# ------------------------------------------------------------------------------
#' @title Add Text Annotation
#'
#' @description Adds a text annotation to a Cytoscape network view. The object 
#' will also be added to the Annotation Panel in the GUI.
#' @param text The text to be displayed
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param fontSize (optional) Numeric value; default is 12
#' @param fontFamily (optional) Font family; default is Arial
#' @param fontStyle (optional) Font style; default is
#' @param color (optional) Hexidecimal color; default is #000000 (black)
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param name (optional) Name of annotation object; default is "Text"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' addAnnotationText("test1")
#' addAnnotationText("test2", 1000, 1000, name="T2")
#' addAnnotationText("test3", 1200, 1000, 30, "Helvetica", "bold", "#990000",
#'     40,name="T3", canvas="foreground",z=4)
#' }
#' @export
addAnnotationText<-function(text = NULL, x.pos = NULL, y.pos = NULL,
                            fontSize = NULL, fontFamily = NULL, fontStyle = NULL,
                            color = NULL, angle = NULL, name = NULL,
                            canvas = NULL, z.order = NULL,
                            network = NULL, base.url = .defaultBaseUrl){

  cmd.string <- 'annotation add text'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.TextAnnotation"')
  
  # text to add
  if(is.null(text))
    stop(simpleError("Must provide the text string to add."))
  cmd.string <- paste0(cmd.string,' text="',text,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(fontSize)){
    .checkPositive(fontSize)
    cmd.string <- paste0(cmd.string,' fontSize="',fontSize,'"')
  }
  if(!is.null(fontFamily)){
    cmd.string <- paste0(cmd.string,' fontFamily="',fontFamily,'"')
  }
  if(!is.null(fontStyle)){
    .checkFontStyle(fontStyle)
    cmd.string <- paste0(cmd.string,' fontStyle="',fontStyle,'"')
  }
  if(!is.null(color)){
    .checkHexColor(color)
    cmd.string <- paste0(cmd.string,' color="',color,'"')
  }
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Add Bounded Text Annotation
#'
#' @description Adds a bounded text annotation to a Cytoscape network view. The 
#' object will also be added to the Annotation Panel in the GUI.
#' @param text The text to be displayed
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param fontSize (optional) Numeric value; default is 12
#' @param fontFamily (optional) Font family; default is Arial
#' @param fontStyle (optional) Font style; default is
#' @param color (optional) Hexidecimal color; default is #000000 (black)
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param customShape (optional) Bounding shape; default is RECTANGLE. See
#'  getNodeShapes() for valid options.
#' @param fillColor (optional) Hexidecimal color; default is #000000 (black)
#' @param opacity (optional) Opacity of fill color. Must be an integer between 
#' 0 and 100; default is 100.
#' @param borderThickness (optional) Integer
#' @param borderColor (optional) Hexidecimal color; default is #000000 (black)
#' @param borderOpacity (optional) Integer between 0 and 100; default is 100.
#' @param height (optional) Height of bounding shape; default is based on text
#' height.
#' @param width (optional) Width of bounding shape; default is based on text
#' length.
#' @param name (optional) Name of annotation object; default is "Text"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' addAnnotationBoundedText("test1")
#' addAnnotationBoundedText("test2", 1000, 1000, name="B2")
#' addAnnotationBoundedText("test3", 1200, 1000, 30, "Helvetica", "bold", "#990000",
#'     40,name="B3", canvas="foreground",z=4)
#' }
#' @export
addAnnotationBoundedText<-function(text = NULL, x.pos = NULL, y.pos = NULL,
                            fontSize = NULL, fontFamily = NULL, fontStyle = NULL,
                            color = NULL, angle = NULL, 
                            customShape = NULL, fillColor = NULL, 
                            opacity = NULL, borderThickness = NULL,
                            borderColor = NULL, borderOpacity = NULL,
                            height = NULL, width = NULL,
                            name = NULL, canvas = NULL, z.order = NULL,
                            network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation add bounded text'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.BoundedTextAnnotation"')
  
  # text to add
  if(is.null(text))
    stop(simpleError("Must provide the text string to add."))
  cmd.string <- paste0(cmd.string,' text="',text,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(fontSize)){
    .checkPositive(fontSize)
    cmd.string <- paste0(cmd.string,' fontSize="',fontSize,'"')
  }
  if(!is.null(fontFamily)){
    cmd.string <- paste0(cmd.string,' fontFamily="',fontFamily,'"')
  }
  if(!is.null(fontStyle)){
    .checkFontStyle(fontStyle)
    cmd.string <- paste0(cmd.string,' fontStyle="',fontStyle,'"')
  }
  if(!is.null(color)){
    .checkHexColor(color)
    cmd.string <- paste0(cmd.string,' color="',color,'"')
  }
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(customShape)){
    customShape <- toupper(customShape)
    if(!customShape %in% getNodeShapes())
      stop (simpleError(sprintf('%s is invalid. Choose a shape from getNodeShapes()',
                        customShape)))
    cmd.string <- paste0(cmd.string,' customShape="',customShape,'"')
  }
  if(!is.null(fillColor)){
    .checkHexColor(fillColor)
    cmd.string <- paste0(cmd.string,' fillColor="',fillColor,'"')
  }
  if(!is.null(opacity)){
    .checkOpacity(opacity)
    cmd.string <- paste0(cmd.string,' opacity="',opacity,'"')
  }
  if(!is.null(borderThickness)){
    if(!is.numeric(borderThickness)){
      if(borderThickness%%1 != 0){
        stop(simpleError('Value must be an integer greater than or equal to 0.'))
      }
    }
    if (borderThickness < 0)
      stop (simpleError(sprintf ('%s is invalid. Value must be an integer greater than or equal to 0.', as.character(borderThickness))))
    cmd.string <- paste0(cmd.string,' borderThickness="',borderThickness,'"')
  }
  if(!is.null(borderColor)){
    .checkHexColor(borderColor)
    cmd.string <- paste0(cmd.string,' borderColor="',borderColor,'"')
  }
  if(!is.null(borderOpacity)){
    .checkOpacity(borderOpacity)
    cmd.string <- paste0(cmd.string,' borderOpacity="',borderOpacity,'"')
  }
  if(!is.null(height)){
    .checkPositive(height)
    cmd.string <- paste0(cmd.string,' height="',height,'"')
  }
  if(!is.null(width)){
    .checkPositive(width)
    cmd.string <- paste0(cmd.string,' width="',width,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Add Image Annotation
#'
#' @description Adds a Image annotation to a Cytoscape network view. The 
#' object will also be added to the Annotation Panel in the GUI.
#' @param url URL or path to image file. File paths can be absolute or relative
#' to current working directory. URLs must start with http:// or https://.
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param opacity (optional) Opacity of fill color. Must be an integer between 
#' 0 and 100; default is 100.
#' @param brightness (optional) Image brightness. Must be an integer between 
#' 0 and 100; default is 100.
#' @param contrast (optional) Image contrast. Must be an integer between 
#' 0 and 100; default is 100.
#' @param borderThickness (optional) Integer
#' @param borderColor (optional) Hexidecimal color; default is #000000 (black)
#' @param borderOpacity (optional) Integer between 0 and 100; default is 100.
#' @param height (optional) Height of image; default is based on text
#' height.
#' @param width (optional) Width of image; default is based on text
#' length.
#' @param name (optional) Name of annotation object; default is "Image"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' addAnnotationImage("image.png")
#' addAnnotationImage("/Users/janedoe/Desktop/image.png", 1000, 1000, name="I2")
#' addAnnotationImage("https://www.example.com/image.png", 1200, 1000, 30, 
#'     40, name="I3", canvas="background",z=4)
#' }
#' @export
addAnnotationImage<-function(url = NULL, x.pos = NULL, y.pos = NULL,
                             angle = NULL, 
                             opacity = NULL, brightness = NULL,
                             contrast = NULL, borderThickness = NULL,
                             borderColor = NULL, borderOpacity = NULL,
                             height = NULL, width = NULL,
                             name = NULL, canvas = NULL, z.order = NULL,
                             network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation add image'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.ImageAnnotation"')
  
  # Image to add
  if(is.null(url))
    stop("URL or path to image file must be provided.")
  if(!grepl("^http[s]*://", url))
    url = paste0("file:",getAbsSandboxPath(url))
  cmd.string <- paste0(cmd.string,' url="',url,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(opacity)){
    .checkOpacity(opacity)
    cmd.string <- paste0(cmd.string,' opacity="',opacity,'"')
  }
  if(!is.null(brightness)){
    .checkBrightnessContrast(brightness)
    cmd.string <- paste0(cmd.string,' brightness="',brightness,'"')
  }
  if(!is.null(contrast)){
    .checkBrightnessContrast(contrast)
    cmd.string <- paste0(cmd.string,' contrast="',contrast,'"')
  }
  if(!is.null(borderThickness)){
    if(!is.numeric(borderThickness)){
      if(borderThickness%%1 != 0){
        stop(simpleError('Value must be an integer greater than or equal to 0.'))
      }
    }
    if (borderThickness < 0)
      stop (simpleError(sprintf ('%s is invalid. Value must be an integer greater than or equal to 0.', as.character(borderThickness))))
    cmd.string <- paste0(cmd.string,' borderThickness="',borderThickness,'"')
  }
  if(!is.null(borderColor)){
    .checkHexColor(borderColor)
    cmd.string <- paste0(cmd.string,' borderColor="',borderColor,'"')
  }
  if(!is.null(borderOpacity)){
    .checkOpacity(borderOpacity)
    cmd.string <- paste0(cmd.string,' borderOpacity="',borderOpacity,'"')
  }
  if(!is.null(height)){
    .checkPositive(height)
    cmd.string <- paste0(cmd.string,' height="',height,'"')
  }
  if(!is.null(width)){
    .checkPositive(width)
    cmd.string <- paste0(cmd.string,' width="',width,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Add Shape Annotation
#'
#' @description Adds a shape annotation to a Cytoscape network view. The 
#' object will also be added to the Annotation Panel in the GUI.
#' @param customShape (optional) Shape; default is RECTANGLE. See
#'  getNodeShapes() for valid options.
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param fillColor (optional) Hexidecimal color; default is #000000 (black)
#' @param opacity (optional) Opacity of fill color. Must be an integer between 
#' 0 and 100; default is 100.
#' @param borderThickness (optional) Integer
#' @param borderColor (optional) Hexidecimal color; default is #000000 (black)
#' @param borderOpacity (optional) Integer between 0 and 100; default is 100.
#' @param height (optional) Height of shape; default is based on text
#' height.
#' @param width (optional) Width of shape; default is based on text
#' length.
#' @param name (optional) Name of annotation object; default is "Shape"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' addAnnotationShape("rectangle")
#' addAnnotationShape("rectangle", 1000, 1000, name="S2")
#' addAnnotationShape("rectangle", 1200, 1000, 30, "#990000",
#'     40,name="S3", canvas="background",z=4)
#' }
#' @export
addAnnotationShape<-function(customShape = NULL, x.pos = NULL, y.pos = NULL,
                                   angle = NULL, fillColor = NULL, 
                                   opacity = NULL, borderThickness = NULL,
                                   borderColor = NULL, borderOpacity = NULL,
                                   height = NULL, width = NULL,
                                   name = NULL, canvas = NULL, z.order = NULL,
                                   network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation add shape'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.ShapeAnnotation"')
  
  # shape to add
  if(!is.null(customShape)){
    customShape <- toupper(customShape)
    if(!customShape %in% getNodeShapes())
      stop (simpleError(sprintf('%s is invalid. Choose a shape from getNodeShapes()',
                                customShape)))
    cmd.string <- paste0(cmd.string,' customShape="',customShape,'"')
  }
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(fillColor)){
    .checkHexColor(fillColor)
    cmd.string <- paste0(cmd.string,' fillColor="',fillColor,'"')
  }
  if(!is.null(opacity)){
    .checkOpacity(opacity)
    cmd.string <- paste0(cmd.string,' opacity="',opacity,'"')
  }
  if(!is.null(borderThickness)){
    if(!is.numeric(borderThickness)){
      if(borderThickness%%1 != 0){
        stop(simpleError('Value must be an integer greater than or equal to 0.'))
      }
    }
    if (borderThickness < 0)
      stop (simpleError(sprintf ('%s is invalid. Value must be an integer greater than or equal to 0.', as.character(borderThickness))))
    cmd.string <- paste0(cmd.string,' borderThickness="',borderThickness,'"')
  }
  if(!is.null(borderColor)){
    .checkHexColor(borderColor)
    cmd.string <- paste0(cmd.string,' borderColor="',borderColor,'"')
  }
  if(!is.null(borderOpacity)){
    .checkOpacity(borderOpacity)
    cmd.string <- paste0(cmd.string,' borderOpacity="',borderOpacity,'"')
  }
  if(!is.null(height)){
    .checkPositive(height)
    cmd.string <- paste0(cmd.string,' height="',height,'"')
  }
  if(!is.null(width)){
    .checkPositive(width)
    cmd.string <- paste0(cmd.string,' width="',width,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Delete Annotation
#'
#' @description Remove an annotation from the current network view in Cytoscape
#' @param names List of annotations by UUID or Name
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @return None
#' @examples \donttest{
#' deleteAnnotation("016a4af1-69bc-4b99-8183-d6f118847f96")
#' deleteAnnotation(c("T2","T3"))
#' deleteAnnotation(sapply(getAnnotationList(), '[[', 'uuid'))
#' }
#' @export
deleteAnnotation<-function(names = NULL, base.url = .defaultBaseUrl){
  if(is.null(names))
    stop('Must provide the UUID (or list of UUIDs) to delete')
  
  if(is.vector(names) ){
    lapply(names, function(u){
      commandsGET(paste0('annotation delete uuidOrName="',u,'"'), base.url)
    })
    invisible()
  }
  
  invisible(commandsGET(paste0('annotation delete uuidOrName="',names,'"'), 
                        base.url))
}

# ------------------------------------------------------------------------------
#' @title Get Annotation List
#'
#' @description A list of named lists with annotation information
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @details You can obtain a list of UUIDs by applying a subset function
#' like so: sapply(getAnnotationList(), '[[', 'uuid')
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAnnotationList()
#' }
#' @export
getAnnotationList<-function(network = NULL, base.url = .defaultBaseUrl){
  cmd.string <- 'annotation list'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view=SUID:"',view.SUID,'"')
  
  commandsPOST(cmd.string, base.url)
}

# ------------------------------------------------------------------------------
#' @title Ungroup Annotation Group
#'
#' @description Ungroup annotation group from the network view in Cytoscape
#' @param names Name of annotation group by UUID or Name
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @details You can obtain a list of UUIDs by applying a subset function
#' like so: sapply(getAnnotationList(), '[[', 'uuid')
#' @return None
#' @examples \donttest{
#' ungroupAnnotation("016a4af1-69bc-4b99-8183-d6f118847f96")
#' ungroupAnnotation(c("316869a4-39fc-4731-8f45-199dec9af10d","c3621eb4-4687-490f-9396-b829dd8767d5"))
#' ungroupAnnotation("Group 1")
#' ungroupAnnotation(c("Group1","Group2", "Group3"))
#' }
#' @export
ungroupAnnotation<-function(names = NULL, network = NULL, base.url = .defaultBaseUrl){
  if(is.null(names))
    stop('Must provide the UUID (or list of UUIDs) to ungroup')
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  if(is.vector(names) ){
    lapply(names, function(u){
      commandsGET(paste0('annotation ungroup uuidOrName="',u,'"', ' view=SUID:"', view.SUID,'"'), base.url)
    })
    invisible()
  }
  
  invisible(commandsGET(paste0('annotation ungroup uuidOrName="',names,'"', ' view=SUID:"',view.SUID,'"', base.url)))
}

# ------------------------------------------------------------------------------
#' @title Group Annotation
#'
#' @description Group annotation from the network view in Cytoscape
#' @param names Name of annotation by UUID or Name
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by 
#' this version of RCy3.
#' @details You can obtain a list of UUIDs by applying a subset function
#' like so: sapply(getAnnotationList(), '[[', 'uuid')
#' @return None
#' @examples \donttest{
#' groupAnnotation(c("29ac8349-7be4-404e-8363-9537cc39d1ad","3846e949-3130-4362-83de-d02f5368e3ad"))
#' groupAnnotation(c("annotation1","annotation2", "annotation3"))
#' groupAnnotation("annotation1")
#' groupAnnotation(sapply(getAnnotationList(), '[[', 'uuid'))
#' }
#' @export
groupAnnotation<-function(names = NULL, network = NULL, base.url = .defaultBaseUrl){
  if(is.null(names))
    stop('Must provide the UUID (or list of UUIDs) to group')
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  names <- paste(names, collapse=', ' )
  
  if(is.vector(names) ){
    lapply(names, function(u){
      commandsGET(paste0('annotation group annotationlist="',u,'"', ' view=SUID:"', view.SUID,'"'), base.url)
    })
    invisible()
  }
  
  invisible(commandsGET(paste0('annotation group annotationlist="',names,'"', ' view=SUID:"',view.SUID,'"', base.url)))
}

# ------------------------------------------------------------------------------
#' @title Update Text Annotation
#'
#' @description Updates a text annotation to a Cytoscape network view. The object 
#' will also be added to the Annotation Panel in the GUI.
#' @param text The text to be displayed
#' @param annotationName Name of annotation by UUID or Name
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param fontSize (optional) Numeric value; default is 12
#' @param fontFamily (optional) Font family; default is Arial
#' @param fontStyle (optional) Font style; default is
#' @param color (optional) Hexidecimal color; default is #000000 (black)
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param name (optional) Name of annotation object; default is "Text"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' updateAnnotationText("test1", "annotationName")
#' updateAnnotationText("test2", "annotationName", 1000, 1000, name="T2")
#' updateAnnotationText("test3", "annotationName", 1200, 1000, 30, "Helvetica", "bold", "#990000",
#'     40,name="T3", canvas="foreground",z=4)
#' }
#' @export
updateAnnotationText<-function(text = NULL, annotationName = NULL, x.pos = NULL, y.pos = NULL,
                            fontSize = NULL, fontFamily = NULL, fontStyle = NULL,
                            color = NULL, angle = NULL, name = NULL,
                            canvas = NULL, z.order = NULL,
                            network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation update text'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.TextAnnotation"')
  
  # text to add
  if(is.null(text))
    stop(simpleError("Must provide the text string to add."))
  cmd.string <- paste0(cmd.string,' text="',text,'"')
  
  if(is.null(annotationName))
    stop('Must provide the UUID (or list of UUIDs) to group')
  cmd.string <- paste0(cmd.string,' uuidOrName="',annotationName,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(fontSize)){
    .checkPositive(fontSize)
    cmd.string <- paste0(cmd.string,' fontSize="',fontSize,'"')
  }
  if(!is.null(fontFamily)){
    cmd.string <- paste0(cmd.string,' fontFamily="',fontFamily,'"')
  }
  if(!is.null(fontStyle)){
    .checkFontStyle(fontStyle)
    cmd.string <- paste0(cmd.string,' fontStyle="',fontStyle,'"')
  }
  if(!is.null(color)){
    .checkHexColor(color)
    cmd.string <- paste0(cmd.string,' color="',color,'"')
  }
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Update Bounded Text Annotation
#'
#' @description Adds a bounded text annotation to a Cytoscape network view. The 
#' object will also be added to the Annotation Panel in the GUI.
#' @param text The text to be displayed
#' @param annotationName Name of annotation by UUID or Name
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param fontSize (optional) Numeric value; default is 12
#' @param fontFamily (optional) Font family; default is Arial
#' @param fontStyle (optional) Font style; default is
#' @param color (optional) Hexidecimal color; default is #000000 (black)
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param customShape (optional) Bounding shape; default is RECTANGLE. See
#'  getNodeShapes() for valid options.
#' @param fillColor (optional) Hexidecimal color; default is #000000 (black)
#' @param opacity (optional) Opacity of fill color. Must be an integer between 
#' 0 and 100; default is 100.
#' @param borderThickness (optional) Integer
#' @param borderColor (optional) Hexidecimal color; default is #000000 (black)
#' @param borderOpacity (optional) Integer between 0 and 100; default is 100.
#' @param height (optional) Height of bounding shape; default is based on text
#' height.
#' @param width (optional) Width of bounding shape; default is based on text
#' length.
#' @param name (optional) Name of annotation object; default is "Text"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' UpdateAnnotationBoundedText("test1", "annotationName")
#' UpdateAnnotationBoundedText("test2", "annotationName", 1000, 1000, name="B2")
#' UpdateAnnotationBoundedText("test3", "annotationName", 1200, 1000, 30, "Helvetica", "bold", "#990000",
#'     40,name="B3", canvas="foreground",z=4)
#' }
#' @export
UpdateAnnotationBoundedText<-function(text = NULL, annotationName= NULL, x.pos = NULL, y.pos = NULL,
                                   fontSize = NULL, fontFamily = NULL, fontStyle = NULL,
                                   color = NULL, angle = NULL, 
                                   customShape = NULL, fillColor = NULL, 
                                   opacity = NULL, borderThickness = NULL,
                                   borderColor = NULL, borderOpacity = NULL,
                                   height = NULL, width = NULL,
                                   name = NULL, canvas = NULL, z.order = NULL,
                                   network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation update bounded text'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.BoundedTextAnnotation"')
  
  # text to add
  if(is.null(text))
    stop(simpleError("Must provide the text string to add."))
  cmd.string <- paste0(cmd.string,' text="',text,'"')
  
  if(is.null(annotationName))
    stop('Must provide the UUID (or list of UUIDs) to group')
  cmd.string <- paste0(cmd.string,' uuidOrName="',annotationName,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(fontSize)){
    .checkPositive(fontSize)
    cmd.string <- paste0(cmd.string,' fontSize="',fontSize,'"')
  }
  if(!is.null(fontFamily)){
    cmd.string <- paste0(cmd.string,' fontFamily="',fontFamily,'"')
  }
  if(!is.null(fontStyle)){
    .checkFontStyle(fontStyle)
    cmd.string <- paste0(cmd.string,' fontStyle="',fontStyle,'"')
  }
  if(!is.null(color)){
    .checkHexColor(color)
    cmd.string <- paste0(cmd.string,' color="',color,'"')
  }
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(customShape)){
    customShape <- toupper(customShape)
    if(!customShape %in% getNodeShapes())
      stop (simpleError(sprintf('%s is invalid. Choose a shape from getNodeShapes()',
                                customShape)))
    cmd.string <- paste0(cmd.string,' customShape="',customShape,'"')
  }
  if(!is.null(fillColor)){
    .checkHexColor(fillColor)
    cmd.string <- paste0(cmd.string,' fillColor="',fillColor,'"')
  }
  if(!is.null(opacity)){
    .checkOpacity(opacity)
    cmd.string <- paste0(cmd.string,' opacity="',opacity,'"')
  }
  if(!is.null(borderThickness)){
    if(!is.numeric(borderThickness)){
      if(borderThickness%%1 != 0){
        stop(simpleError('Value must be an integer greater than or equal to 0.'))
      }
    }
    if (borderThickness < 0)
      stop (simpleError(sprintf ('%s is invalid. Value must be an integer greater than or equal to 0.', as.character(borderThickness))))
    cmd.string <- paste0(cmd.string,' borderThickness="',borderThickness,'"')
  }
  if(!is.null(borderColor)){
    .checkHexColor(borderColor)
    cmd.string <- paste0(cmd.string,' borderColor="',borderColor,'"')
  }
  if(!is.null(borderOpacity)){
    .checkOpacity(borderOpacity)
    cmd.string <- paste0(cmd.string,' borderOpacity="',borderOpacity,'"')
  }
  if(!is.null(height)){
    .checkPositive(height)
    cmd.string <- paste0(cmd.string,' height="',height,'"')
  }
  if(!is.null(width)){
    .checkPositive(width)
    cmd.string <- paste0(cmd.string,' width="',width,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

# ------------------------------------------------------------------------------
#' @title Update Shape Annotation
#'
#' @description Updates a shape annotation to a Cytoscape network view. The 
#' object will also be added to the Annotation Panel in the GUI.
#' @param customShape (optional) Shape; default is RECTANGLE. See
#'  getNodeShapes() for valid options.
#' @param x.pos (optional) X position in pixels from left; default is center 
#' of current view
#' @param y.pos (optional) Y position in pixels from top; default is center 
#' of current view
#' @param angle (optional) Angle of text orientation; default is 0.0 
#' (horizontal)
#' @param fillColor (optional) Hexidecimal color; default is #000000 (black)
#' @param opacity (optional) Opacity of fill color. Must be an integer between 
#' 0 and 100; default is 100.
#' @param borderThickness (optional) Integer
#' @param borderColor (optional) Hexidecimal color; default is #000000 (black)
#' @param borderOpacity (optional) Integer between 0 and 100; default is 100.
#' @param height (optional) Height of shape; default is based on text
#' height.
#' @param width (optional) Width of shape; default is based on text
#' length.
#' @param name (optional) Name of annotation object; default is "Shape"
#' @param canvas (optional) Canvas to display annotation, i.e., foreground 
#' (default) or background
#' @param z.order (optional) Arrangement order specified by number (larger
#' values are in front of smaller values); default is 0 
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported by
#' this version of RCy3.
#' @return A named list of annotation properties, including UUID
#' @examples \donttest{
#' updateAnnotationShape("rectangle", "annotationName")
#' updateAnnotationShape("rectangle", "annotationName", 1000, 1000, name="S2")
#' updateAnnotationShape("rectangle", "annotationName", 1200, 1000, 30, "#990000",
#'     40,name="S3", canvas="background",z=4)
#' }
#' @export
updateAnnotationShape<-function(customShape = NULL, annotationName = NULL, x.pos = NULL, y.pos = NULL,
                             angle = NULL, fillColor = NULL, 
                             opacity = NULL, borderThickness = NULL,
                             borderColor = NULL, borderOpacity = NULL,
                             height = NULL, width = NULL,
                             name = NULL, canvas = NULL, z.order = NULL,
                             network = NULL, base.url = .defaultBaseUrl){
  
  cmd.string <- 'annotation update shape'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')
  
  # add type
  cmd.string <- paste0(cmd.string, ' type="org.cytoscape.view.presentation.annotations.ShapeAnnotation"')
  
  # shape to add
  if(!is.null(customShape)){
    customShape <- toupper(customShape)
    if(!customShape %in% getNodeShapes())
      stop (simpleError(sprintf('%s is invalid. Choose a shape from getNodeShapes()',
                                customShape)))
    cmd.string <- paste0(cmd.string,' customShape="',customShape,'"')
  }
  if(is.null(annotationName))
    stop('Must provide the UUID (or list of UUIDs) to group')
  cmd.string <- paste0(cmd.string,' uuidOrName="',annotationName,'"')
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(fillColor)){
    .checkHexColor(fillColor)
    cmd.string <- paste0(cmd.string,' fillColor="',fillColor,'"')
  }
  if(!is.null(opacity)){
    .checkOpacity(opacity)
    cmd.string <- paste0(cmd.string,' opacity="',opacity,'"')
  }
  if(!is.null(borderThickness)){
    if(!is.numeric(borderThickness)){
      if(borderThickness%%1 != 0){
        stop(simpleError('Value must be an integer greater than or equal to 0.'))
      }
    }
    if (borderThickness < 0)
      stop (simpleError(sprintf ('%s is invalid. Value must be an integer greater than or equal to 0.', as.character(borderThickness))))
    cmd.string <- paste0(cmd.string,' borderThickness="',borderThickness,'"')
  }
  if(!is.null(borderColor)){
    .checkHexColor(borderColor)
    cmd.string <- paste0(cmd.string,' borderColor="',borderColor,'"')
  }
  if(!is.null(borderOpacity)){
    .checkOpacity(borderOpacity)
    cmd.string <- paste0(cmd.string,' borderOpacity="',borderOpacity,'"')
  }
  if(!is.null(height)){
    .checkPositive(height)
    cmd.string <- paste0(cmd.string,' height="',height,'"')
  }
  if(!is.null(width)){
    .checkPositive(width)
    cmd.string <- paste0(cmd.string,' width="',width,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, vapply(getAnnotationList(), '[[', 'character', 'name'))
    cmd.string <- paste0(cmd.string,' newName="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', 
                                 z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}
