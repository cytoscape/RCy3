# ==============================================================================
# I. Sandbox functions
# ------------------------------------------------------------------------------
#' @title sandboxSet
#'
#' @description Set a new default sandbox, creating it if necessary.
#' A sandbox is the root for the file system used for all file operations. When running standalone
#' on the same workstation as Cytoscape, the default sandbox is the directory that's current for
#' the R kernel. When running in a Notebook or remote server, the default sandbox is the
#' 'default_sandbox' created automatically under the under the filetransfer directory in the
#' CytoscapeConfiguration directory. Naming a sandbox with this function creates a new
#' sub-directory as a sibling to 'default_sandbox' and uses it for subsequent file operations.
#' Setting a None sandbox uses the default sandbox instead.
#' Sandboxes are highly recommended as an aid to creating workflows that can be shared with
#' others.
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
#' @description Delete sandbox contents and remove its directory.
#' If the current sandbox is the entire file system on a Cytoscape workstation, trying to delete it
#' is an error. Otherwise, deleting the current sandbox results in the default sandbox becoming the
#' new current sandbox. When running standalone on the same workstation as Cytoscape, the default
#' sandbox is the entire file system on the Cytoscape workstation. When running in a Notebook or
#' remote server, the default sandbox is the 'default_sandbox' created automatically under the
#' under the filetransfer directory in the CytoscapeConfiguration directory. If that sandbox is
#' deleted, it will be re-created so that subsequent file operations can complete successfully.
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
    res <- .sandboxOp('filetransfer removeSandbox', sandboxName, base.url=base.url)
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
#' @description Get metadata on file in sandbox (or entire sandbox).
#' If the current sandbox is the entire file system on a Cytoscape workstation, trying to delete it
#' is an error. Otherwise, deleting the current sandbox results in the default sandbox becoming the
#' new current sandbox. When running standalone on the same workstation as Cytoscape, the default
#' sandbox is the entire file system on the Cytoscape workstation. When running in a Notebook or
#' remote server, the default sandbox is the 'default_sandbox' created automatically under the
#' under the filetransfer directory in the CytoscapeConfiguration directory. If that sandbox is
#' deleted, it will be re-created so that subsequent file operations can complete successfully.
#' Note that this function can be used to query either a file or a directory.
#' @param fileName Name of file whose metadata to return ... can be sandbox-relative path ... ``.`` returns metadata on sandbox itself
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <full path on Cytoscape workstation>, 'modifiedTime': <last changed time, '' if file doesn't exist>, 'isFile': <True if file, False if directory>}
#' @examples \donttest{
#' sandboxGetFileInfo()
#' }
#' @importFrom utils file_test
#' @export
sandboxGetFileInfo <- function(fileName, sandboxName=NULL, base.url=.defaultBaseUrl){
    tryCatch(
        expr = {
            return(.sandboxOp('filetransfer getFileInfo', sandboxName, fileName=fileName, base.url=base.url))
        },
        error = function(e){
            if(is.null(sandboxName) && is.null(getCurrentSandboxName()) && !is.null(fileName) && !is.null(trimws(fileName))){
                filePath <- (file.path(getwd(), fileName)) 
                if (file.exists(filePath)){
                    isFile <- utils::file_test("-f", fileName)
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
#' @description Transfer a file to a sandbox.
#' The source file is transferred to the named (or current) sandbox, overwriting an existing file if one
#' already exists. The destFile can be an absolute path if the sandbox is the entire file system (i.e., for
#' standalone R execution) or a path relative to the sandbox (i.e., for Notebook or remote execution or if a
#' sandbox was explicitly created).
#' Note that there is no function that transfers an entire directory. Note, though, that when using sandboxSet()
#' to make a sandbox current, it is possible to copy the Cytoscape sample data directories into to the sandbox at the
#' same time.
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
#' @import base64enc
#' @importFrom base64url base64_urlencode
#' @export
sandboxSendTo <- function(sourceFile, destFile=NULL, overwrite=TRUE, sandboxName=NULL, base.url=.defaultBaseUrl){
    tryCatch(
        expr = {
            finfo = file.info(sourceFile)
            read.filename <- file(sourceFile, "rb")
            fileContent <- try(readChar(read.filename, file.info(sourceFile)$size), silent = TRUE)
            if(inherits(fileContent, "try-error")){
                fileContent64 <- base64encode(sourceFile)
                fileByteCount <- finfo$size
            } else {
                fileContent64 <- base64_urlencode(fileContent)
                fileByteCount <- nchar(fileContent)
            }
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
    res <- .sandboxOp(glue('filetransfer toSandbox fileByteCount={fileByteCount} overwrite={overwrite}, fileBase64="{fileContent64}"'), sandboxName, fileName=destFile, base.url=base.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxUrlTo
#'
#' @description Transfer a cloud-based file to a sandbox.
#' The source URL identifies a file to be transferred to the named (or current) sandbox, overwriting an existing
#' file if one already exists. The destFile can be an absolute path if the sandbox is the entire file
#' system (i.e., for standalone R execution), or it can be a path relative to the sandbox (i.e., for Notebook or
#' remote execution or if a sandbox was explicitly created).                                                                                     
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
    res <- .sandboxOp(glue('filetransfer urlToSandbox overwrite={overwrite} sourceURL={sourceURL}'), sandboxName, fileName=destFile, base.url=base.url)
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxGetFrom
#'
#' @description Transfer a file from a sandbox.
#' The source file is transferred from the named (or current) sandbox to the R workflow's file system,
#' overwriting an existing file if one already exists. The sourceFile can be an absolute path if the sandbox is
#' the entire file system (i.e., for standalone R execution) or a path relative to the sandbox
#' (i.e., for Notebook or remote execution or if a sandbox was explicitly created).
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
#' @import base64enc
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
        errorMessage <- "File already exists"
        stop(glue(errorMessage))
    }
    res <- .sandboxOp('filetransfer fromSandbox', sandboxName, fileName=sourceFile, base.url=base.url)
    fileContent <- res$fileBase64
    tryCatch(
        expr = {
            outconn <- file(destFile,"wb")
            base64decode(what=fileContent, output=outconn)
            close(outconn)
        },
        error = function(e){
            message(glue('Could not write to file {destFile}'))
            print(e)
        }
    )
    res[['fileBase64']] <- NULL
    return(res)
}

# ------------------------------------------------------------------------------
#' @title sandboxRemoveFile
#'
#' @description Remove a file from a sandbox.
#' The named file is removed from the named sandbox. If the sandbox is the entire file system (i.e., for standalone
#' R execution), the file name can be an absolute path. Otherwise, it is a path relative to the named sandbox.
#' Note that there is no function that deletes a directory, except for sandboxRemove(), which deletes a sandbox
#' and all of its contents.
#' @param fileName Name of file to delete (as absolute path or sandbox-relative path)
#' @param sandboxName Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <file's absolute path in Cytoscape workstation>, 'existed': True if file existed before being deleted}
#' @examples \donttest{
#' sandboxRemoveFile()
#' }
#' @export
sandboxRemoveFile <- function(fileName, sandboxName=NULL, base.url=.defaultBaseUrl){
    return(.sandboxOp('filetransfer removeFile', sandboxName, fileName=fileName, base.url=base.url))
}

# ------------------------------------------------------------------------------
# title .sandboxOp
# description internal function for sandbox operation
# param command command
# param sandboxName Name of file to read (as absolute path or sandbox-relative path)
# param fileName Name of file to read (as absolute path or sandbox-relative path)
# param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
# return sandbox result
.sandboxOp <- function(command, sandboxName, fileName=NULL, base.url=.defaultBaseUrl){
    if(!is.null(fileName)){
        fileName <- trimws(fileName)
    }
    if(!is.null(sandboxName)){
        sandboxName <- trimws(sandboxName)
        sandboxPath <- getCurrentSandboxPath()
    } else {
        box <- doInitializeSandbox(base.url=base.url)
        sandboxName <- box[[1]]
        sandboxPath <- box[[2]]
    }
    if(!is.null(sandboxName)){
        command <- paste(command, sprintf("sandboxName=%s", sandboxName))
    } else if(!is.null(fileName)){
        if(isAbsolutePath(fileName)){
            fileName <- fileName
        } else {
            fileName <- file.path(sandboxPath, fileName)
        }
    }
    if(!is.null(fileName)){
        command <- paste(command, sprintf("fileName=%s", fileName))
    }
    res <- commandsPOST(command, base.url = base.url)
    return(res)
}