#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R Internal.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/diffusion/v1/52/views/772/diffuse
setGeneric ('diffuse', function (obj, title=NA, view=NA) standardGeneric('diffuse'))

##http://localhost:1234/v1/commands/cybrowser/dialog 
setGeneric ('cyBrowser', function (obj, title, url, text=NA, id=NA, debug=FALSE) standardGeneric('cyBrowser'))
setGeneric ('cyBrowserHide', function (obj, id=NA) standardGeneric('cyBrowserHide'))
setGeneric ('cyBrowserShow', function (obj, id=NA) standardGeneric('cyBrowserShow'))
setGeneric ('cyBrowserVersion', function (obj) standardGeneric('cyBrowserVersion'))

# ------------------------------------------------------------------------------
