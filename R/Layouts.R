#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 

# ------------------------------------------------------------------------------
setGeneric ('getLayoutNames', 	         signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLayoutNames'))
setGeneric ('getLayoutNameMapping',	     signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLayoutNameMapping'))
setGeneric ('getLayoutPropertyNames',    signature='obj', function (obj=CytoscapeConnection(), layout.name) standardGeneric ('getLayoutPropertyNames'))
setGeneric ('getLayoutPropertyType',     signature='obj', function (obj=CytoscapeConnection(), layout.name, property.name) standardGeneric ('getLayoutPropertyType'))
setGeneric ('getLayoutPropertyValue',    signature='obj', function (obj=CytoscapeConnection(), layout.name, property.name) standardGeneric ('getLayoutPropertyValue'))
setGeneric ('setLayoutProperties',       signature='obj', function (obj=CytoscapeConnection(), layout.name, properties.list) standardGeneric ('setLayoutProperties'))
setGeneric ('layoutNetwork',             signature='obj', function (obj=CytoscapeWindowFromNetwork(), layout.name=NA) standardGeneric ('layoutNetwork'))
setGeneric ('saveLayout',           	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, timestamp.in.filename=FALSE) standardGeneric ('saveLayout'))
setGeneric ('restoreLayout',        	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename) standardGeneric ('restoreLayout'))


# ------------------------------------------------------------------------------
setMethod('getLayoutNames', 'OptionalCyObjClass', 
          function(obj) {
              request.uri <- paste(obj@uri, obj@api, "apply/layouts", sep="/")
              request.res <- GET(url=request.uri)
              
              available.layouts <- unname(fromJSON(rawToChar(request.res$content)))
              return(available.layouts)
          }) 
## END getLayoutNames

# ------------------------------------------------------------------------------
setMethod('getLayoutNameMapping', 'OptionalCyObjClass', 
          function(obj) {
              layout.names <- getLayoutNames(obj)
              layout.full.names <- c()
              
              # get the English/full name of a layout
              for (layout.name in layout.names){
                  request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), sep="/")
                  request.res <- GET(url=request.uri)
                  
                  layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
                  layout.full.names <- c(layout.full.names, layout.property.names[[4]])
              }
              names(layout.names) <- layout.full.names
              
              return(layout.names)
          })
## END getLayoutNameMapping

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyNames', 'OptionalCyObjClass', 
          function(obj, layout.name) {
              request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
              request.res <- GET(url=request.uri)
              
              layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
              return(sapply(layout.property.names, '[[', 1))
          })
## END getLayoutPropertyNames

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyType', 'OptionalCyObjClass', 
          function(obj, layout.name, property.name) {
              request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
              request.res <- GET(url=request.uri)
              
              layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
              layout.property.names <- sapply(layout.property.list, '[[', 1)
              position <- layout.property.names == property.name
              return(sapply(layout.property.list, '[[', 3)[position])
          }) 
## END getLayoutPropertyType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyValue', 'OptionalCyObjClass', 
           
           function (obj, layout.name, property.name) {
               request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
               request.res <- GET(url=request.uri)
               
               layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
               layout.property.names <- sapply(layout.property.list, '[[', 1)
               position <- layout.property.names == property.name
               return(sapply(layout.property.list, '[[', 4)[position])
           }) # getLayoutPropertyValue

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setLayoutProperties', 'OptionalCyObjClass', 
           
           function (obj, layout.name, properties.list) {
               all.possible.props <- getLayoutPropertyNames (obj, layout.name)
               
               # set properties iteratively, this could have been done with a single API call
               for (prop in names (properties.list)) {
                   if (!prop %in% all.possible.props) {
                       write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
                   } else {
                       new.value <- properties.list [[prop]]
                       new.property.value.list <- list("name"=prop, "value"=new.value)
                       new.property.value.list.JSON <- toJSON(list(new.property.value.list))
                       
                       request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
                       request.res <- PUT(url=request.uri, body= new.property.value.list.JSON, encode="json")
                       if (request.res$status == 200){
                           write (sprintf ("Successfully updated the property '%s'.", prop), stdout ())
                       } else {
                           write (sprintf ("Something went wrong. Unable to update property '%s'.", prop), stderr ())
                       }
                       invisible(request.res)
                   }
               } # for prop
           }) # setLayoutProperties

# ------------------------------------------------------------------------------
setMethod('layoutNetwork', 'OptionalCyWinClass', 
          function(obj, layout.name = NA) {
              if(is.na(layout.name)){
                  commandRun('layout apply preferred networkSelected="current"', obj=obj) #TODO: update network handling
                  return("Applied preferred layout")
              }
              if(!layout.name %in% getLayoutNames(obj)) {
                  write(sprintf("layout.name '%s' is not recognized; call getLayoutNames(<CytoscapeWindow>) to see those which are supported", layout.name), stderr())
              }
              id = as.character(obj@suid)
              
              api.str <- paste(obj@uri, obj@api, "apply/layouts", layout.name, id, sep = "/")
              
              res <- GET(api.str)
              invisible(res)
          }) # layoutNetwork

#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveLayout', 'OptionalCyWinClass',
           
           function (obj, filename, timestamp.in.filename=FALSE) {
               
               custom.layout <- RCy3::getNodePosition (obj,  getAllNodes (obj))
               if (timestamp.in.filename) {
                   dateString <- format (Sys.time (), "%a.%b.%d.%Y-%H.%M.%S")
                   stem <- strsplit (filename, '\\.RData')[[1]]
                   filename <- sprintf ('%s.%s.RData', stem, dateString)
                   write (sprintf ('saving layout to %s\n', filename), stderr ())
               }
               save (custom.layout, file=filename)
           }) # save.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('restoreLayout', 'OptionalCyWinClass',
           
           function (obj, filename) {
               custom.layout <- local({x=load(filename); get(x)})
               node.names <- names (custom.layout)
               node.names.filtered <- intersect (node.names, getAllNodes (obj))
               x <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$x)))
               y <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$y)))
               setNodePosition (obj, node.names.filtered, x, y)
           }) # restoreLayout

