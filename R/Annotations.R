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
    cmd.string <- paste0(cmd.string,' name="',name,'"')
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
