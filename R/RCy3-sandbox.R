defaultSandbox = list() # Once a sandbox is explicitly defined, it'll override this default
defaultSandboxPath <- NULL
predefinedSandboxName <- 'default_sandbox'
currentSandboxName <- NULL
currentSandboxPath <- NULL # Resolve this by explicitly setting it or when first Cytoscape command is issued
sandboxReinitialize <- TRUE

sandboxTemplate <- list('sandboxName' = NULL,  'copySamples' = TRUE, 'reinitialize' = TRUE)

# ------------------------------------------------------------------------------
#' @title sandboxInitializer
#'
#' @description sandboxInitializer.
#' @return sandbox
#' @examples \donttest{
#' sandboxInitializer()
#' }
sandboxInitializer <- function(newSandbox=NULL, ...){
    if (length(newSandbox) == 1 && !is.null(newSandbox[['init']])){
        params <- newSandbox[['init']]
    } else{
        params <- newSandbox
    }
    sandbox <- sandboxTemplate
    return(sandbox)
}
# ------------------------------------------------------------------------------