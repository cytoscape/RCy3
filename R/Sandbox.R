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
    tryCatch(
        expr = {
            return(sandboxOp('filetransfer getFileInfo', sandboxName, fileName, base.url=base.url))
        },
        error = function(e){
            if(is.null(sandboxName) && is.null(getCurrentSandboxName()) && !is.null(fileName) && !is.null(trimws(fileName))){
                filePath <- normalizePath(fileName)
                if (file.exists(filePath)){
                    isFile <- file_test("-f", fileName)
                    modifiedTime <- format(file.info(fileName)$mtime, usetz=FALSE)
                } else {
                    isFile <- FALSE
                    modifiedTime <- ''
                }
                return(list('filePath' = filePath,  'modifiedTime' = modifiedTime, 'isFile' = isFile))
            } else {
                stop()
            }
        }
    )
}

# ------------------------------------------------------------------------------
#' @title sandboxSendTo
#'
#' @description sandboxSendTo
#' @return sandboxSendTo
#' @examples \donttest{
#' sandboxSendTo()
#' }
#' @export
sandboxSendTo <- function(sourceFile, destFile=NULL, overwrite=TRUE, sandboxName=NULL, base.url=base.url){
    
}

# ------------------------------------------------------------------------------
#' @title sandboxGetFrom
#'
#' @description sandboxGetFrom
#' @param sourceFile Name of file to read (as absolute path or sandbox-relative path)
#' @param destFile  Name of file in the R workflow's file system ... if None, use file name in source_file
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return sandboxGetFrom
#' @examples \donttest{
#' sandboxGetFrom()
#' }
#' @import glue
#' @export
sandboxGetFrom <- function(sourceFile, destFile=NULL, overwrite=TRUE, sandboxName=NULL, base.url=base.url){
    if(!is.null(sourceFile)){
        sourceFile <- trimws(sourceFile)
    } else {
        sourceFile <- ''
    }
    if(!overwrite && file.exists(destFile)){
        errorMessage <- "File {destFile} already exists"
        stop(glue(errorMessage))
    }
    res <- sandboxOp('filetransfer fromSandbox', sandboxName, fileName=sourceFile, base.url=base.url)
    
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
