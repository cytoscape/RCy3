# ------------------------------------------------------------------------------
#' @title sandboxInitializer
#'
#' @description Start with a sandbox template and update properties using whatever is found in the new_sandbox.
#' @param newSandbox newSandbox
#' @param ... ...
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
#' @description Set and return the sandbox properties to be used as a default, probably based on whether a Notebook is running.
#' @param newSandbox newSandbox
#' @param ... ...
#' @return default sandbox
#' @examples \donttest{
#' setDefaultSandbox()
#' }
#' @export
setDefaultSandbox <- function(newSandbox=NULL, ...){
    RCy3env$.defaultSandbox <- sandboxInitializer(newSandbox)
    return(RCy3env$.defaultSandbox)
}

# ------------------------------------------------------------------------------
#' @title getDefaultSandbox
#'
#' @description Return whatever is the current default sandbox properties.
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
#' @description Set and return the default path, which isn't one of the properties tracked in the default_sandbox.
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
#' @description Return the default path, which isn't one of the properties tracked in the default_sandbox.
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
#' @description Return the current sandbox name.
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
#' @description Return the current sandbox path.
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
#' @description Return both the current sandbox name and path.
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
#' @description Set and return the current sandbox name and path.
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
#' @description Set and return flag indicating that next command should reinitialize the sandbox according to the default_sandbox.
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
#' @description Return flag indicating that next command should reinitialize the sandbox according to the default_sandbox.
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
#' @description Get absolute sandbox path.
#' @param fileLocation fileLocation 
#' @return file location
#' @examples \donttest{
#' getAbsSandboxPath()
#' }
#' @export
getAbsSandboxPath <- function(fileLocation){
    box <- getCurrentSandbox()
    boxName <- box[[1]]
    boxPath <- box[[2]]
    if (is.null(boxName)){
        if(isAbsolutePath(fileLocation)){
            filePath <- fileLocation
        } else {
            filePath <- (file.path(getwd(), fileLocation)) 
        } 
        return(filePath)
    } else if (!is.null(boxName) && !is.null(boxPath)){
        return(paste(boxPath, fileLocation, sep="/"))
    } else {
        return(fileLocation)
    }
}

# ------------------------------------------------------------------------------
#' @title resetDefaultSandbox
#'
#' @description Reset the entire state of the sandbox system.
#' @return None
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