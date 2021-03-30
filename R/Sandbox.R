# ==============================================================================
# ------------------------------------------------------------------------------
#' @title sandboxSet
#'
#' @description sandboxSet
#' @return sandboxPath
#' @examples \donttest{
#' sandboxSet()
#' }
#' @export
sandboxSet <- function(sandboxName, copySamples=TRUE, reinitialize=TRUE, base.url=.defaultBaseUrl){
    if(!is.null(sandboxName)){
        sandboxName = trimws(sandboxName)
        box <- doSetSandbox(list('sandboxName' = sandboxName,  'copySamples' = copySamples, 'reinitialize' = reinitialize))
        boxName <- box[1]
        boxPath <- box[2]
        return(boxPath)
    }
}

# ------------------------------------------------------------------------------
#' @title sandboxRemove
#'
#' @description sandboxRemove
#' @return sandboxRemove
#' @examples \donttest{
#' sandboxRemove()
#' }
#' @export
sandboxRemove <- function(sandboxName=NULL, base.url=.defaultBaseUrl){
    if(!is.null(sandboxName)){
        sandboxName <- trimws(sandboxName)
    }
    defaultSandboxName <- getDefaultSandbox()['sandboxName']
    currentSandboxBeforeRemove <- getCurrentSandboxName()
    res <- sandboxOp('filetransfer removeSandbox', sandboxName, base.url=base.url)
    if(is.null(sandboxName) || sandboxName == currentSandboxBeforeRemove){
        setCurrentSandbox(defaultSandboxName, getDefaultSandboxPath())
        sandboxName <- currentSandboxBeforeRemove
    }
    if(sandboxName == defaultSandboxName && defaultSandboxName == currentSandboxBeforeRemove){
        setSandboxReinitialize()
    } else if (sandboxName == currentSandboxBeforeRemove){
        doSetSandbox(list('sandboxName' = defaultSandboxName,  'copySamples' = FALSE, 'reinitialize' = FALSE))
    }
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxGetFileInfo
#'
#' @description sandboxGetFileInfo
#' @return sandboxGetFileInfo
#' @examples \donttest{
#' sandboxGetFileInfo()
#' }
#' @export
sandboxGetFileInfo <- function(fileName, sandboxName=NULL, base.url=.defaultBaseUrl){
    
}

# ------------------------------------------------------------------------------
#' @title sandboxRemoveFile
#'
#' @description sandboxRemoveFile
#' @return sandboxRemoveFile
#' @examples \donttest{
#' sandboxRemoveFile()
#' }
#' @export
sandboxRemoveFile <- function(fileName, sandboxName=NULL, base.url=.defaultBaseUrl){
    return(sandboxOp('filetransfer removeFile', sandboxName, fileName=fileName, base.url=base.url))
}

# ------------------------------------------------------------------------------
#' @title sandboxOp
#'
#' @description sandboxOp
#' @return sandboxOp
#' @examples \donttest{
#' sandboxOp()
#' }
#' @export
sandboxOp <- function(command, sandboxName, fileName=NULL, base.url=.defaultBaseUrl){
    if(!is.null(fileName)){
        fileName <- trimws(fileName)
    }
    if(!is.null(sandboxName)){
        sandboxName <- trimws(sandboxName)
        sandboxPath <- getCurrentSandboxPath()
    } else {
        box <- doInitializeSandbox(base.url=base.url)
        sandboxName <- box[1]
        sandboxPath <- box[2]
    }
    if(!is.null(sandboxName)){
        command <- paste(command, sprintf(" sandboxName=%s", sandboxName))
    } else if(!is.null(fileName)){
        fileName <- file.path(sandboxPath, fileName)
    }
    if(!is.null(fileName)){
        command <- paste(command, sprintf(" fileName=%s", fileName))
    }
    res <- commandsPOST(command, base.url = base.url)
    return(res)
}
