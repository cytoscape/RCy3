# ------------------------------------------------------------------------------
#' @importFrom dplR uuid.gen
ug <- uuid.gen()
uuid <- character(1)
uuid[1] <- ug()
CHANNEL <- uuid[1]
# ------------------------------------------------------------------------------
#' @title spoofResponse
#' @description class
#' @import jsonlite
#' @export
spoofResponse <- setClass(
    "spoofResponse",
    slots = c(
        URL = "ANY",
        status_code = "ANY",
        Reason = "ANY",
        Text = "ANY"
    )
)

setGeneric("repr", function(object, ...) standardGeneric("repr"))
setGeneric("jsonLoads", function(object, ...) standardGeneric("jsonLoads"))
setGeneric("raiseForStatus", function(object, ...) standardGeneric("raiseForStatus"))

setMethod(f="initialize", signature="spoofResponse",
          definition=function(.Object) {
              .Object@URL <- "https://jupyter-bridge.cytoscape.org"
              .Object@status_code <- "0"
              .Object@Reason <- "reason"
              .Object@Text <- "text"
              return(.Object)
          }
)

setMethod("repr", "spoofResponse", function(object, ...) {
    dput(object@status_code)
}) 

setMethod("jsonLoads", "spoofResponse", function(object, ...) {
    spoofResponsetext <- fromJSON(object@Text)
    return(spoofResponsetext)
}) 

setMethod("raiseForStatus", "spoofResponse", function(object, ...) {
    if(object@status_code < 500 & object@status_code >= 400){
        stop("Client Error")
    }
    else if(object@status_code < 600 & object@status_code >= 500){
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
#' @import uchardet
#' @export
doRequestRemote<-function(method, qurl, qbody=NULL, headers=NULL){
    tryCatch(
        expr = {
            request <- list(command = method, url = qurl, data = qbody, headers=list("Content-Type" = "application/json", "Accept" = "application/json"))
            url_post <- sprintf('%s/queue_request?channel=%s',JupyterBRIDGEURL, CHANNEL)
            r <- POST(url_post, body = request, encode="json", content_type_json(), add_headers("Content-Type" = "application/json"))
        },
        error = function(e){
            message('Error posting to Jupyter-bridge!')
            print(e)
        }
    )
    tryCatch(
        expr = {
            while (TRUE){
                url_get <- sprintf('%s/dequeue_reply?channel=%s',JupyterBRIDGEURL, CHANNEL)
                r <- GET(url_get, accept_json())
                if(status_code(r) != 408){break}
            }
        },
        error = function(e){
            message('Error receiving from Jupyter-bridge!')
            print(e)
        }        
    )
    tryCatch(
        expr = {
            rContent <- content(r, "text")
            encoding <- detect_str_enc(rContent)
            message <- toString((iconv(rContent, to=encoding)))
            cyReply <- fromJSON(message)
        },
        error = function(e){
            message('Undeciperable message received from Jupyter-bridge!')
            print(e)
        }
    )
    rsp = spoofResponse()
    if (cyReply[1] == 0){ 
        stop("Could not contact url")
    }
    rsp@status_code <- cyReply[1]
    rsp@Reason <- cyReply[2]
    rsp@Text <- cyReply[3]
    #print(rsp)
    return(r)
}
# ------------------------------------------------------------------------------
#' @title setNotebookIsRunning
#' @description setNotebookIsRunning
#' @examples \donttest{
#' setNotebookIsRunning()
#' }
runningRemote <- NULL
notebookIsRunning <- NULL
#' @export
setNotebookIsRunning<-function(newState=NULL){
    oldState <- .GlobalEnv$notebookIsRunning
    .GlobalEnv$notebookIsRunning <- newState
    return(oldState)
}
# ------------------------------------------------------------------------------
#' @title getNotebookIsRunning
#' @description getNotebookIsRunning
#' @examples \donttest{
#' getNotebookIsRunning()
#' }
#' @export
getNotebookIsRunning<-function(){
    return(.GlobalEnv$notebookIsRunning)
}
# ------------------------------------------------------------------------------
#' @title checkNotebookIsRunning
#' @description checkNotebookIsRunning
#' @examples \donttest{
#' checkNotebookIsRunning()
#' }
#' @export
checkNotebookIsRunning<-function(){
    if(is.null(getNotebookIsRunning())){
        if(getOption("jupyter.in_kernel")){
            setNotebookIsRunning(TRUE)
        }
        else if(!getOption("jupyter.in_kernel")){
            setNotebookIsRunning(FALSE)
        }
        else{
            setNotebookIsRunning(FALSE)
        }
    }
}
# ------------------------------------------------------------------------------
#' @title runningRmoteCheck
#' @description runningRmoteCheck
#' @examples \donttest{
#' runningRmoteCheck()
#' }
#' @export
runningRemoteCheck<-function(newState=NULL){
    checkRunningRemote()
    oldState <- .GlobalEnv$runningRemote
    if(!is.null(newState)){
        .GlobalEnv$runningRemote <- newState
    }
    return(oldState)
}
# ------------------------------------------------------------------------------
#' @title checkRunningRemote
#' @description checkRunningRemote
#' @examples \donttest{
#' checkRunningRemote()
#' }
#' @import httr
#' @export
checkRunningRemote<-function(){
    tryCatch(
        expr={
    if(getNotebookIsRunning()){
        if(is.null(.GlobalEnv$runningRemote)){
            tryCatch(
                expr = {
                    r <- GET(url='http://127.0.0.1:1234/v1')
                    status_code(r)
                    .GlobalEnv$runningRemote <- FALSE
                },
                error = function(e){
                    tryCatch(
                        expr = {
                    doRequestRemote("GET", 'http://127.0.0.1:1234/v1')
                    .GlobalEnv$runningRemote <- TRUE},
                    error = function(e){
                        message('Error initially contacting Jupyter-bridge!')
                        print(e)
                        .GlobalEnv$runningRemote <- NULL
                    }
                    )
                }
            )
        }
    }else{
        .GlobalEnv$runningRemote <- FALSE 
    }
        },
    error = function(e){
        checkNotebookIsRunning()
        checkRunningRemote()
    })
    return(.GlobalEnv$runningRemote)
}
# ------------------------------------------------------------------------------