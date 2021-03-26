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