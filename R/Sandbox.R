# ==============================================================================
# I. Sandbox functions
# ------------------------------------------------------------------------------
#' @title sandboxSet
#'
#' @description sandboxSet
#' @param sandboxName Name of new default sandbox. None means to use the original default sandbox 
#' @param copySamples True to copy the Cytoscape sampleData into the sandbox
#' @param reinitialize True to delete sandbox contents (if any) if sandbox already exists
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return sandbox path in Cytoscape workstation's file system
#' @examples \donttest{
#' sandboxSet()
#' }
#' @export
sandboxSet <- function(sandboxName, copySamples=TRUE, reinitialize=TRUE, base.url=.defaultBaseUrl){
    if(!is.null(sandboxName)){
        sandboxName = trimws(sandboxName)
    }
    box <- doSetSandbox(list('sandboxName'=sandboxName, 'copySamples'=copySamples, 'reinitialize'=reinitialize), base.url=base.url)
    boxName <- box[1]
    boxPath <- box[2]
    return(boxPath)
}

# ------------------------------------------------------------------------------
#' @title sandboxRemove
#'
#' @description sandboxRemove
#' @param sandboxName Name of sandbox to delete. None means to delete the current sandbox. If that sandbox is the default sandbox, it is automatically re-created.
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'sandboxPath': <directory on Cytoscape workstation>, 'existed': <True if sandbox existed>}
#' @examples \donttest{
#' sandboxRemove()
#' }
#' @export
sandboxRemove <- function(sandboxName=NULL, base.url=.defaultBaseUrl){
    if(!is.null(sandboxName)){
        sandboxName <- trimws(sandboxName)
    }
    defaultSandboxName <- getDefaultSandbox()[['sandboxName']]
    currentSandboxBeforeRemove <- getCurrentSandboxName()
    res <- sandboxOp('filetransfer removeSandbox', sandboxName, base.url=base.url)
    if(is.null(sandboxName) || sandboxName == currentSandboxBeforeRemove){
        setCurrentSandbox(defaultSandboxName, getDefaultSandboxPath())
        sandboxName <- currentSandboxBeforeRemove
    }
    if(sandboxName == defaultSandboxName && defaultSandboxName == currentSandboxBeforeRemove){
        setSandboxReinitialize()
    } else if (sandboxName == currentSandboxBeforeRemove){
        doSetSandbox(list('sandboxName'=defaultSandboxName, 'copySamples'=FALSE, 'reinitialize'=FALSE))
    }
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxGetFileInfo
#'
#' @description sandboxGetFileInfo
#' @param fileName Name of file whose metadata to return ... can be sandbox-relative path ... ``.`` returns metadata on sandbox itself
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <full path on Cytoscape workstation>, 'modifiedTime': <last changed time, '' if file doesn't exist>, 'isFile': <True if file, False if directory>}
#' @examples \donttest{
#' sandboxGetFileInfo()
#' }
#' @export
sandboxGetFileInfo <- function(fileName, sandboxName=NULL, base.url=.defaultBaseUrl){
    tryCatch(
        expr = {
            return(sandboxOp('filetransfer getFileInfo', sandboxName, fileName=fileName, base.url=base.url))
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
                return(list('filePath'=filePath, 'modifiedTime'=modifiedTime, 'isFile'=isFile))
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
#' @param sourceFile Name of file to read (as absolute path or sandbox-relative path)
#' @param destFile Name of file in the R workflow's file system ... if None, use file name in source_file
#' @param overwrite Name of sandbox containing file. None means "the current sandbox".
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return sandboxSendTo
#' @examples \donttest{
#' sandboxSendTo()
#' }
#' @import glue
#' @import RCurl
#' @export
sandboxSendTo <- function(sourceFile, destFile=NULL, overwrite=TRUE, sandboxName=NULL, base.url=.defaultBaseUrl){
    tryCatch(
        expr = {
            finfo = file.info(sourceFile)
            read.filename <- file(sourceFile, "rb")
            fileContent <- readChar(read.filename, file.info(sourceFile)$size)
            fileContent64 <- base64Encode(fileContent)
            close(read.filename)
        },
        error = function(e){
            message(glue('Could not read file {sourceFile}'))
            print(e)
        }
    )
    if(is.null(destFile) || is.null(trimws(destFile))){
        head <- dirname(sourceFile)
        destFile <- basename(sourceFile)
    }
    res <- sandboxOp(glue('filetransfer toSandbox fileByteCount={finfo$size} overwrite={overwrite}, fileBase64="{fileContent64}"'), sandboxName, fileName=destFile, base.url=base.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxUrlTo
#'
#' @description sandboxUrlTo
#' @param sourceURL URL addressing cloud file to download
#' @param destFile Name of file in the R workflow's file system ... if None, use file name in source_file
#' @param overwrite Name of sandbox containing file. None means "the current sandbox".
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <new file's absolute path in Cytoscape workstation>, 'fileByteCount': number of bytes read}
#' @examples \donttest{
#' sandboxUrlTo()
#' }
#' @import glue
#' @import RCurl
#' @export
sandboxUrlTo <- function(sourceURL, destFile, overwrite=TRUE, sandboxName=NULL, base.url=.defaultBaseUrl){
    if(is.null(sourceURL)){
        stop('Source URL cannot be null')
    }
    if(is.null(destFile)){
        stop('Destination file cannot be null')
    }
    res <- sandboxOp(glue('filetransfer urlToSandbox overwrite={overwrite} sourceURL={sourceURL}'), sandboxName, fileName=destFile, base.url=basr.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxGetFrom
#'
#' @description sandboxGetFrom
#' @param sourceFile Name of file to read (as absolute path or sandbox-relative path)
#' @param destFile Name of file in the R workflow's file system ... if None, use file name in source_file
#' @param overwrite Name of sandbox containing file. None means "the current sandbox".
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return sandboxGetFrom
#' @examples \donttest{
#' sandboxGetFrom()
#' }
#' @import glue
#' @import RCurl
#' @export
sandboxGetFrom <- function(sourceFile, destFile=NULL, overwrite=TRUE, sandboxName=NULL, base.url=.defaultBaseUrl){
    if(!is.null(sourceFile)){
        sourceFile <- trimws(sourceFile)
    } else {
        sourceFile <- ''
    }
    if(is.null(destFile) || is.null(trimws(destFile))){
        head <- dirname(sourceFile)
        destFile <- basename(sourceFile)
    }
    if(!overwrite && file.exists(destFile)){
        errorMessage <- "File {destFile} already exists"
        stop(glue(errorMessage))
    }
    res <- sandboxOp('filetransfer fromSandbox', sandboxName, fileName=sourceFile, base.url=base.url)
    fileContent <- base64Decode(res$fileBase64)
    tryCatch(
        expr = {
            write.filename <- file(destFile, "wb")
            writeChar(fileContent, write.filename)
            close(write.filename)
        },
        error = function(e){
            message(glue('Could not write to file {destFile}'))
            print(e)
        }
    )
    res[4] <- NULL
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxRemoveFile
#'
#' @description sandboxRemoveFile
#' @param fileName Name of file to delete (as absolute path or sandbox-relative path)
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <file's absolute path in Cytoscape workstation>, 'existed': True if file existed before being deleted}
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
#' @param command command
#' @param sandboxName Name of file to read (as absolute path or sandbox-relative path)
#' @param fileName Name of file to read (as absolute path or sandbox-relative path)
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return sandbox result
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
        command <- paste(command, sprintf("sandboxName=%s", sandboxName))
    } else if(!is.null(fileName)){
        fileName <- file.path(sandboxPath, fileName)
    }
    if(!is.null(fileName)){
        command <- paste(command, sprintf("fileName=%s", fileName))
    }
    print(command)
    res <- commandsPOST(command, base.url = base.url)
    return(res)
}
