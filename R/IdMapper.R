#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R Internal.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1/commands/idmapper/map%20column 
setGeneric ('mapIdentifiers', function (obj, title=NA, table=NA, column.name, species, mapFrom, mapTo, force.single=TRUE) standardGeneric('mapIdentifiers'))

