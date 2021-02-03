# ------------------------------------------------------------------------------
#' @title spoofResponse
#' @description class
#' @import uchardet
#' @import jsonlite
#' @export
spoofResponse <- setClass(
    "spoofResponse",
    slots = c(
        selfURL = "ANY",
        selfStatusCode = "ANY",
        selfReason = "ANY",
        selfText = "ANY"
    )
)

setGeneric("repr", function(object, ...) standardGeneric("repr"))
setGeneric("jsonLoads", function(object, ...) standardGeneric("jsonLoads"))
setGeneric("raiseForStatus", function(object, ...) standardGeneric("raiseForStatus"))

setMethod(f="initialize", signature="spoofResponse",
          definition=function(.Object) {
              .Object@selfURL <- "jupyter-bridge"
              .Object@selfStatusCode <- "0"
              .Object@selfReason <- "reason"
              .Object@selfText <- "text"
              return(.Object)
          }
)

setMethod("repr", "spoofResponse", function(object, ...) {
    dput(object@selfStatusCode)
}) 

setMethod("jsonLoads", "spoofResponse", function(object, ...) {
    spoofResponsetext <- fromJSON(object@selfText)
    return(spoofResponsetext)
}) 

setMethod("raiseForStatus", "spoofResponse", function(object, ...) {
    if(object@selfStatusCode < 500 & object@selfStatusCode >= 400){
        stop("Client Error")
    }
    else if(object@selfStatusCode < 600 & object@selfStatusCode >= 500){
        stop("Server Error")
    }
}) 



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
    tryCatch(
        expr = {
            x <- list(command = "GET", url = "http://127.0.0.1:1234/v1/version")
            http_request <- toJSON(x)
            url_post <- sprintf('%s/queue_request?channel=%s',JupyterBRIDGEURL, CHANNEL)
            r <- POST(url_post, content_type_json(), body = http_request)
            print(status_code(r))
        },
        error = function(e){
            message('Error posting to Jupyter-bridge!')
            print(e)
        }
    )
    tryCatch(
        expr = {
            #while (TRUE){
                url_get <- sprintf('%s/dequeue_reply?channel=%s',JupyterBRIDGEURL, CHANNEL)
                r <- GET(url_get)
                #if(status_code(r) != 408){break}
            },
        #},
        error = function(e){
            message('Error receiving from Jupyter-bridge!')
            print(e)
        }        
    )
    tryCatch(
        expr = {
            rContent <- content(r, "text")
            encoding <- detect_str_enc(rContent)
            message <- str(iconv(rContent, to=encoding))
            cyReply <- fromJSON(message)
            print(cyReply)
        },
        error = function(e){
            message('Undeciperable message received from Jupyter-bridge!')
            print(e)
        }
    )
    r = spoofResponse()
    r@selfStatusCode <- cyReply
    r@selfReason <- cyReply
    r@selfText <- cyReply
    return(r)
}
# ------------------------------------------------------------------------------
