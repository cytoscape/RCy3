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
        return(sandboxName)
    }
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
