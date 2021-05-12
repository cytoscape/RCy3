# ==============================================================================
# Internal utility functions used by more than one RCy3 function. These should
# not be exported, nor visible to package users. Add variable and functions here 
# if you suspect they will be useful for other developers. 
# 
# Dev Note: internal variables and functions should be prefixed with a '.'
# ==============================================================================
# I. Package Variables and Constants
# ------------------------------------------------------------------------------
.defaultBaseUrl <- 'http://127.0.0.1:1234/v1'

RCy3env <- new.env()
# Exported setter functions for these delays are in RCy3.R
assign(".CATCHUP_FILTER_SECS", 1, envir = RCy3env)
assign(".MODEL_PROPAGATION_SECS", 5, envir = RCy3env)
assign(".CATCHUP_NETWORK_SECS", 2, envir = RCy3env)
assign(".defaultSandbox", list(), envir = RCy3env)
assign(".defaultSandboxPath", NULL, envir = RCy3env)
assign(".predefinedSandboxName", 'default_sandbox', envir = RCy3env)
assign(".currentSandboxName", NULL, envir = RCy3env)
assign(".currentSandboxPath", NULL, envir = RCy3env)
assign(".sandboxReinitialize", TRUE, envir = RCy3env)
assign(".sandboxTemplate", list('sandboxName' = NULL,  'copySamples' = TRUE, 'reinitialize' = TRUE), envir = RCy3env)

# ==============================================================================
# I. Package Utility Functions
# ------------------------------------------------------------------------------
# Supply a set of colors from Brewer palettes (without requiring rColorBrewer)
.cyPalette <- function(name='set1'){
    
    set1<-c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
              "#A65628", "#F781BF", "#999999")
    set2<-c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
              "#E5C494", "#B3B3B3")
    set3<-c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
              "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5","#FFED6F")
    reds<-c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C",
            "#CB181D", "#A50F15", "#67000D")
    rdbu<-c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
            "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    burd<-rev(rdbu)
    
    pal<-eval(parse(text = name))
    return(pal)
}
# ------------------------------------------------------------------------------
# Validate and provide user feedback when hex color codes are required input.
.checkHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        stop (simpleError(sprintf ('%s is not a valid hexadecimal color, e.g. #FD39B8.', color)))
    }
}

# ------------------------------------------------------------------------------
# Validate and provide user feedback when opacity value is outside of range.
.checkOpacity <- function(opacity){
    if(is.numeric(opacity)){
        if(opacity%%1 != 0){
            stop(simpleError('Opacity must be an integer between 0 and 255.'))
        }
    } else {
        stop(simpleError('Opacity must be an integer between 0 and 255.'))
    }
    if (opacity < 0 || opacity > 255){
        stop (simpleError(sprintf ('%i is invalid. Opacity must be between 0 and 255.', opacity)))
    } 
}

# ------------------------------------------------------------------------------
# Validates and fixes rotation values from -180 to +180 range to match GUI
.normalizeRotation <- function(degree){
    if(!is.numeric(degree))
        stop(simpleError('Angle must be a number.'))
    while (degree <= -180) 
        degree <- degree + 360
    while (degree > 180)
        degree <- degree - 360
    return(degree)
}
# ------------------------------------------------------------------------------
# Validates unique value against provided set
.checkUnique <- function(value, existing.values){
    if(value %in% existing.values)
        stop(simpleError(sprintf ('%s is not unique. Please provide a unique value.', as.character(value))))
}
# ------------------------------------------------------------------------------
# Validates positive number
.checkPositive <- function(number){
    if(!is.numeric(number))
        stop(simpleError('Value must be a positive number.'))
    if (number <= 0){
        stop (simpleError(sprintf ('%s is invalid. Number must be positive.', as.character(number))))
    } 
}
# ------------------------------------------------------------------------------
# Validates acceptable font style
.checkFontStyle <- function(style){
    if(!style %in% c("plain","bold","italic","bolditalic"))
        stop (simpleError(sprintf ('%s is invalid. Use "plain", "bold", "italic" or "bolditalic"', style)))
}
# ------------------------------------------------------------------------------
# Validates acceptable canvas
.checkCanvas <- function(canvas){
    if(!canvas %in% c("foreground","background"))
        stop (simpleError(sprintf ('%s is invalid. Use "foreground" or "background"', canvas)))
}

# ------------------------------------------------------------------------------
# Validate and provide user feedback when slot number is outside of range.
.checkSlot <- function(slot){
    if(is.numeric(slot)){
        if(slot%%1 != 0){
            stop(simpleError('Slot must be an integer between 1 and 9.'))
        }
    } else {
        stop(simpleError('Slot must be an integer between 1 and 9.'))
    }
    if (!slot %in% seq_len(9)){
        stop (simpleError(sprintf('%i is invalid. Slot must be an integer between 1 and 9.', slot)))
    }
}

# ------------------------------------------------------------------------------
# Replaces node names with SUIDs.
.nodeNameToNodeSUID<-function(node.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        test <- vapply(node.names, function(x){x %in% dict[,'SUID']}, logical(1))
        if(all(test))  #provided SUIDs already!
            return(node.names)
        sorted.dict <- NULL
        if(length(node.names) == length(unique(node.names))){ #unique node names
            sorted.dict <- dict[match(node.names, dict$name), ] 
        } else { #multiple nodes with the same name
            message("Finding unique SUIDs for nodes with the same name...\n")
            match_list <- list()
            for(i in seq_along(node.names)){ #perform match with removal
                name_match <- dict[match(node.names[[i]], dict$name),]
                match_list[[i]] <- name_match
                dict <- subset(dict, SUID != name_match$SUID)
            }
            sorted.dict <- do.call(rbind, match_list)
        }
        node.SUIDs <- sorted.dict$SUID
        return(node.SUIDs)
}
# ------------------------------------------------------------------------------
# Replaces node SUIDs with names.
.nodeSUIDToNodeName<-function(node.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        test <- vapply(node.suids, function(x){x %in% dict[,'name']}, logical(1))
        if(all(test)) #provided names already!
            return(node.suids)
        node.names <- dict$name[match(node.suids, dict$SUID)]
        return(node.names)
}

# ------------------------------------------------------------------------------
# Replaces edge names with SUIDs.
.edgeNameToEdgeSUID<-function(edge.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        test <- vapply(edge.names, function(x){x %in% dict[,'SUID']}, logical(1))
        if(all(test))  #provided SUIDs already!
            return(edge.names)
        sorted.dict <- NULL
        if(length(edge.names) == length(unique(edge.names))){ #unique edge names
            sorted.dict <- dict[match(edge.names, dict$name), ] 
        } else { #multigraph: multiple edges with the same name
            message("Finding unique SUIDs for edges with the same name...\n")
            match_list <- list()
            for(i in seq_along(edge.names)){ #perform match with removal
                name_match <- dict[match(edge.names[[i]], dict$name),]
                match_list[[i]] <- name_match
                dict <- subset(dict, SUID != name_match$SUID)
            }
            sorted.dict <- do.call(rbind, match_list)
        }
        edge.SUIDs <- sorted.dict$SUID
        return(edge.SUIDs)
}

# ------------------------------------------------------------------------------
# Replaces edge SUIDs with names.
.edgeSUIDToEdgeName<-function(edge.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        test <- vapply(edge.suids, function(x){x %in% dict[,'name']}, logical(1))
        if(all(test)) #provided names already!
            return(edge.suids)
        edge.names <- dict$name[match(edge.suids, dict$SUID)]
        return(edge.names)
}

# ------------------------------------------------------------------------------
# Checks to see if a particular column name exists in the specific table. Returns
# TRUE or FALSE.
.tableColumnExists <- function(table.column, table, network=network, base.url=base.url){
    if (!table.column %in% getTableColumnNames(table, network=network, base.url=base.url)) {
        message (sprintf ('Column %s does not exist in the %s table.', table.column, table))
        return (FALSE)
    }
    return (TRUE)
}

# ------------------------------------------------------------------------------
# Checks to see if min supported versions of api and cytoscape are running.
# Extracts numerics from api and major cytoscape versions before making comparison.
.verifySupportedVersions<-function(cyrest=1,cytoscape=3.6,base.url=.defaultBaseUrl) {
    vStr <- cytoscapeVersionInfo(base.url)
    vApiStr <- unname(vStr[1])
    vCyStr <- unname(vStr[2])
    vApiNum <- as.numeric(gsub("v([0-9]+)$", "\\1", vApiStr))
    vCyNum <- as.numeric(gsub("([0-9]+\\.[0-9]+)\\..*$", "\\1", vCyStr))

    nogo <- FALSE
    if(cyrest > vApiNum){
        message(sprintf("CyREST API version %d or greater is required. You are currently working with version %d.",
                      cyrest, vApiNum))
        nogo <- TRUE
    }
    if(cytoscape > vCyNum){
        message(sprintf("Cytoscape version %0.2g or greater is required. You are currently working with version %0.2g.",
                      cytoscape, vCyNum))
        nogo <- TRUE
    }
    if(nogo)
        stop(simpleError("Function not run due to unsupported version."))
}
# ------------------------------------------------------------------------------
# Internal function for deleteDuplicateEdges in NetworkSelection.R.
# Convert edge list into list of parts: ["xxx (pp) yyy", "zzz (pp) aaa"] into [("xxx", "pp", "yyy"), ("zzz", "pp", "aaa")]
.parseEdges <- function(edgeList){
    splitEdge <- function(edge){
        res1 <- gsub('(.*) \\((.*)\\) (.*)', "\\1", regmatches(edge,gregexpr('(.*) \\((.*)\\) (.*)',edge)))
        res2 <- gsub('(.*) \\((.*)\\) (.*)', "\\2", regmatches(edge,gregexpr('(.*) \\((.*)\\) (.*)',edge)))
        res3 <- gsub('(.*) \\((.*)\\) (.*)', "\\3", regmatches(edge,gregexpr('(.*) \\((.*)\\) (.*)',edge)))
        return(list(res1, res2, res3))
    }
    return(lapply(edgeList, splitEdge))
}
# ------------------------------------------------------------------------------