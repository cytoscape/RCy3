defaultSandbox <<- list() # Once a sandbox is explicitly defined, it'll override this default
defaultSandboxPath <<- NULL
predefinedSandboxName <<- 'default_sandbox'
currentSandboxName <<- NULL
currentSandboxPath <<- NULL # Resolve this by explicitly setting it or when first Cytoscape command is issued
sandboxReinitialize <<- TRUE

sandboxTemplate <- list('sandboxName' = NULL,  'copySamples' = TRUE, 'reinitialize' = TRUE)

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
    .GlobalEnv$defaultSandbox <- sandboxInitializer(init=newSandbox)
    return(.GlobalEnv$defaultSandbox)
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
    return(.GlobalEnv$defaultSandbox)
}

# ------------------------------------------------------------------------------
#' @title setDefaultSandboxPath
#'
#' @description setDefaultSandboxPath
#' @return default sandbox path
#' @examples \donttest{
#' setDefaultSandboxPath()
#' }
#' @export
setDefaultSandboxPath <- function(newPath){
    .GlobalEnv$defaultSandboxPath <- newPath
    return(.GlobalEnv$defaultSandboxPath)
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
    return(.GlobalEnv$defaultSandboxPath)
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
    return(.GlobalEnv$currentSandboxName)
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
    return(.GlobalEnv$currentSandboxPath)
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
    currentSandbox <- list("currentSandboxName" = .GlobalEnv$currentSandboxName, "currentSandboxPath" = .GlobalEnv$currentSandboxPath)
    return(currentSandbox)
}

# ------------------------------------------------------------------------------
#' @title setCurrentSandbox
#'
#' @description setCurrentSandbox
#' @return current sandbox
#' @examples \donttest{
#' setCurrentSandbox()
#' }
#' @export
setCurrentSandbox <- function(sandboxName, sandboxPath){
    .GlobalEnv$currentSandboxName <- sandboxName
    .GlobalEnv$currentSandboxPath <- sandboxPath
    return(getCurrentSandbox())
}

# ------------------------------------------------------------------------------
#' @title setSandboxReinitialize
#'
#' @description setSandboxReinitialize
#' @return sandbox reinitialize
#' @examples \donttest{
#' setCurrentSandbox()
#' }
#' @export
setSandboxReinitialize <- function(doReinitialize=TRUE){
    .GlobalEnv$sandboxReinitialize <- doReinitialize
    return(.GlobalEnv$sandboxReinitialize)
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
    return(.GlobalEnv$sandboxReinitialize)
}

# ------------------------------------------------------------------------------
#' @title getAbsSandboxPath
#'
#' @description getAbsSandboxPath
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
        return(paste(boxPath, fileLocation,sep="/"))
    } else {
        return(fileLocation)
    }
}

# ------------------------------------------------------------------------------
#' @title resetDefaultSandbox
#'
#' @description resetDefaultSandbox
#' @return NULL
#' @examples \donttest{
#' resetDefaultSandbox()
#' }
#' @export
resetDefaultSandbox <- function(){
    .GlobalEnv$defaultSandbox <- list()
    .GlobalEnv$defaultSandboxPath <- NULL
    setCurrentSandbox(NULL, NULL)
    .GlobalEnv$sandboxReinitialize <- TRUE
}

# ------------------------------------------------------------------------------