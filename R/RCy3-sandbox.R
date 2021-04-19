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
    sandbox <- RCy3env$.sandboxTemplate
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
    RCy3env$.defaultSandbox <- sandboxInitializer(init=newSandbox)
    return(RCy3env$.defaultSandbox)
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
    return(RCy3env$.defaultSandbox)
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
    RCy3env$.defaultSandboxPath <- newPath
    return(RCy3env$.defaultSandboxPath)
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
    return(RCy3env$.defaultSandboxPath)
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
    return(RCy3env$.currentSandboxName)
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
    return(RCy3env$.currentSandboxPath)
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
    currentSandbox <- list("currentSandboxName" = RCy3env$.currentSandboxName, "currentSandboxPath" = RCy3env$.currentSandboxPath)
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
    RCy3env$.currentSandboxName <- sandboxName
    RCy3env$.currentSandboxPath <- sandboxPath
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
    RCy3env$.sandboxReinitialize <- doReinitialize
    return(RCy3env$.sandboxReinitialize)
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
    return(RCy3env$.sandboxReinitialize)
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
    RCy3env$.defaultSandbox <- list()
    RCy3env$.defaultSandboxPath <- NULL
    setCurrentSandbox(NULL, NULL)
    RCy3env$.sandboxReinitialize <- TRUE
}

# ------------------------------------------------------------------------------