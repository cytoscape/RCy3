# ------------------------------------------------------------------------------
#' @title getBrowserClientChannel
#'
#' @description Get the unique channel
#' @return client channel
#' @examples \donttest{
#' getBrowserClientChannel()
#' }
#' @importFrom dplR uuid.gen
ug <- uuid.gen()
uuid <- character(1)
uuid[1] <- ug()
CHANNEL <- uuid[1]
#' @export
getBrowserClientChannel<-function(){
    return(CHANNEL)
}

# ------------------------------------------------------------------------------
#' @title getJupyterBridgeURL
#'
#' @description Get the jupyter bridge server url
#' @return jupyter bridge server url
#' @examples \donttest{
#' getJupyterBridgeURL()
#' }
JupyterBRIDGEURL <- 'https://jupyter-bridge.cytoscape.org'
#' @export
getJupyterBridgeURL<-function(){
    return(JupyterBRIDGEURL)
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
#' @title doRequestRemote
#'
#' @description Do requests remotely
#' @examples \donttest{
#' doRequestRemote()
#' }
#' @import httr
#' @export
doRequestRemote<-function(){
    x <- list(command = "GET", url = "http://127.0.0.1:1234/v1/version")
    http_request <- toJSON(x)
    url_post <- sprintf('%s/queue_request?channel=%s',JupyterBRIDGEURL, CHANNEL)
    POST(url_post, content_type_json(), body = http_request)
    url_get <- sprintf('%s/dequeue_reply?channel=%s',JupyterBRIDGEURL, CHANNEL)
    r1 <- GET(url_get)
    r1 <- content(r1, "text")
    return(r1)
}
# ------------------------------------------------------------------------------
