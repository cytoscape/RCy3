# ------------------------------------------------------------------------------
.defaultSandbox <- list('sandboxName' = "testsandbox",  'copySamples' = TRUE, 'reinitialize' = TRUE)
.defaultSandboxPath <- NULL
.predefinedSandboxName <- 'default_sandbox'
.currentSandboxName <- NULL
.currentSandboxPath <- NULL # Resolve this by explicitly setting it or when first Cytoscape command is issued
.sandboxReinitialize <- TRUE
.sandboxTemplate <- list('sandboxName' = NULL,  'copySamples' = TRUE, 'reinitialize' = TRUE)
# ------------------------------------------------------------------------------
#' @title sandboxInitializer
#'
#' @description sandboxInitializer
#' @return sandbox
#' @examples \donttest{
#' sandboxInitializer()
#' }
#' @export
sandboxInitializer <- function(newSandbox=NULL, ...){
    resetDefaultSandbox()
    if (length(newSandbox) == 1 && !is.null(newSandbox[['init']])){
        params <- newSandbox[['init']]
    } else{
        params <- newSandbox
    }
    sandbox <- sandboxTemplate
    for (name in names(params)) {
        if(name %in% names(sandbox)){
            sandbox[name] <- params[[name]]
        } else {
            stop('Invalid key in sandbox parameter list')
        }
    }
    return(sandbox)
}

# ------------------------------------------------------------------------------
#' @title setDefaultSandbox
#'
#' @description setDefaultSandbox
#' @return default sandbox
#' @examples \donttest{
#' setDefaultSandbox()
#' }
#' @export
setDefaultSandbox <- function(newSandbox=NULL, ...){
    .defaultSandbox <- sandboxInitializer(init=newSandbox)
    return(.defaultSandbox)
}

# ------------------------------------------------------------------------------
#' @title getDefaultSandbox
#'
#' @description getDefaultSandbox
#' @return default sandbox
#' @examples \donttest{
#' getDefaultSandbox()
#' }
#' @export
getDefaultSandbox <- function(){
    return(.defaultSandbox)
}

# ------------------------------------------------------------------------------
#' @title setDefaultSandboxPath
#'
#' @description setDefaultSandboxPath
#' @param newPath new path of default sandbox 
#' @return default sandbox path
#' @examples \donttest{
#' setDefaultSandboxPath()
#' }
#' @export
setDefaultSandboxPath <- function(newPath){
    .defaultSandboxPath <- newPath
    return(.defaultSandboxPath)
}

# ------------------------------------------------------------------------------
#' @title getDefaultSandboxPath
#'
#' @description getDefaultSandboxPath
#' @return default sandbox path
#' @examples \donttest{
#' getDefaultSandboxPath()
#' }
#' @export
getDefaultSandboxPath <- function(){
    return(.defaultSandboxPath)
}

# ------------------------------------------------------------------------------
#' @title getCurrentSandboxName
#'
#' @description getCurrentSandboxName
#' @return current sandbox name
#' @examples \donttest{
#' getCurrentSandboxName()
#' }
#' @export
getCurrentSandboxName <- function(){
    return(.currentSandboxName)
}

# ------------------------------------------------------------------------------
#' @title getCurrentSandboxPath
#'
#' @description getCurrentSandboxPath
#' @return current sandbox path
#' @examples \donttest{
#' getCurrentSandboxPath()
#' }
#' @export
getCurrentSandboxPath <- function(){
    return(.currentSandboxPath)
}

# ------------------------------------------------------------------------------
#' @title getCurrentSandbox
#'
#' @description getCurrentSandbox
#' @return current sandbox
#' @examples \donttest{
#' getCurrentSandbox()
#' }
#' @export
getCurrentSandbox <- function(){
    currentSandbox <- list("currentSandboxName" = .currentSandboxName, "currentSandboxPath" = .currentSandboxPath)
    return(currentSandbox)
}

# ------------------------------------------------------------------------------
#' @title setCurrentSandbox
#'
#' @description setCurrentSandbox
#' @param sandboxName sandboxName 
#' @param sandboxPath sandboxPath 
#' @return current sandbox
#' @examples \donttest{
#' setCurrentSandbox()
#' }
#' @export
setCurrentSandbox <- function(sandboxName, sandboxPath){
    .currentSandboxName <- sandboxName
    .currentSandboxPath <- sandboxPath
    return(getCurrentSandbox())
}

# ------------------------------------------------------------------------------
#' @title setSandboxReinitialize
#'
#' @description setSandboxReinitialize
#' @param doReinitialize default is TRUE 
#' @return sandbox reinitialize
#' @examples \donttest{
#' setCurrentSandbox()
#' }
#' @export
setSandboxReinitialize <- function(doReinitialize=TRUE){
    .sandboxReinitialize <- doReinitialize
    return(.sandboxReinitialize)
}

# ------------------------------------------------------------------------------
#' @title getSandboxReinitialize
#'
#' @description getSandboxReinitialize
#' @return sandbox reinitialize
#' @examples \donttest{
#' getSandboxReinitialize()
#' }
#' @export
getSandboxReinitialize <- function(){
    return(.sandboxReinitialize)
}

# ------------------------------------------------------------------------------
#' @title getAbsSandboxPath
#'
#' @description getAbsSandboxPath
#' @param fileLocation fileLocation 
#' @return file location
#' @examples \donttest{
#' getAbsSandboxPath()
#' }
#' @export
getAbsSandboxPath <- function(fileLocation){
    box <- getCurrentSandbox()
    boxName <- box[1]
    boxPath <- box[2]
    if (is.null(boxName)){
        return(normalizePath(fileLocation))
    } else if (!is.null(boxName) && !is.null(boxPath)){
        return(paste(boxPath, fileLocation, sep="/"))
    } else {
        return(fileLocation)
    }
}

# ------------------------------------------------------------------------------
#' @title resetDefaultSandbox
#'
#' @description resetDefaultSandbox
#' @examples \donttest{
#' resetDefaultSandbox()
#' }
#' @export
resetDefaultSandbox <- function(){
    .defaultSandbox <- list()
    .defaultSandboxPath <- NULL
    setCurrentSandbox(NULL, NULL)
    .sandboxReinitialize <- TRUE
}

# ------------------------------------------------------------------------------