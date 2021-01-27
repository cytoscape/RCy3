# ------------------------------------------------------------------------------
#' @title getBrowserClientChannel
#'
#' @description Get the unique channel
#' @return client channel
#' @examples \donttest{
#' getBrowserClientChannel()
#' }
#' @importFrom uuid UUIDgenerate
#' @export
CHANNEL <- UUIDgenerate()

getBrowserClientChannel<-function(){
    print(CHANNEL)
}

# ------------------------------------------------------------------------------
#' @title getJupyterBridgeURL
#'
#' @description Get the jupyter bridge server url
#' @return jupyter bridge server url
#' @examples \donttest{
#' getJupyterBridgeURL()
#' }
#' @export
JupyterBRIDGEURL <- 'https://jupyter-bridge.cytoscape.org'

getJupyterBridgeURL<-function(){
    print(JupyterBRIDGEURL)
}

# ------------------------------------------------------------------------------
#' @title getBrowserClientJs
#'
#' @description Prepend channel number of client Javascript so it can communicate with this process via Jupyter-bridge
#' @return Javascript inject code
#' @examples \donttest{
#' getBrowserClientJs()
#' }
#' @importFrom httr GET
#' @importFrom httr content
#' @export
getBrowserClientJs<-function(){
    r <- GET("https://raw.githubusercontent.com/cytoscape/jupyter-bridge/master/client/javascript_bridge.js")
    injectCode <- sprintf('var Channel = "%s"; \n\n var JupyterBridge = "%s"; \n\n %s',CHANNEL, JupyterBRIDGEURL, content(r, 'text') )
    return(injectCode)
}

# ------------------------------------------------------------------------------
