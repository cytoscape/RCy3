# ==============================================================================
# Functions related to TOOLS found in the Tools Menu in Cytoscape.
# 
# ------------------------------------------------------------------------------
#' @title Cybrowser Close
#'
#' @description Close an internal web browser and remove all content. Provide an 
#' id for the browser you want to close.
#' @param id (optional) The identifier for the browser window to close
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' cybrowserClose('554')
#' }
#' @export
cybrowserClose <- function(id=NULL, base.url = .defaultBaseUrl){
    id.str <- ""
    if(!is.null(id))
        id.str <- paste0(' id="',id,'"')
    
    commandsPOST(paste0('cybrowser close',id.str),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser Dialog
#'
#' @description Launch Cytoscape's internal web browser in a separate window. 
#' Provide an id for the window if you want subsequent control of the window e.g., 
#' via cybrowser hide.
#' @param id (optional) The identifier for the browser window
#' @param text (optional) HTML text to initially load into the browser
#' @param title (optional) Text to be shown in the title bar of the browser window 
#' @param url (optional) The URL the browser should load
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' cybrowserDialog(url='http://cytoscape.org')
#' }
#' @seealso \link{cybrowserShow}
#' @seealso \link{cybrowserHide}
#' @export
cybrowserDialog <- function(id=NULL, text=NULL, title=NULL, url=NULL, 
                            base.url = .defaultBaseUrl){
    id.str <- ""
    if(!is.null(id))
        id.str <- paste0(' id="',id,'"')
    
    text.str <- ""
    if(!is.null(text))
        text.str <- paste0(' text="',text,'"')
    
    title.str <- ""
    if(!is.null(title))
        title.str <- paste0(' title="',title,'"')
    
    url.str <- ""
    if(!is.null(url))
        url.str <- paste0(' url="',url,'"')
    
    commandsPOST(paste0('cybrowser dialog',id.str,text.str,title.str,url.str),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser Hide
#'
#' @description Hide an existing browser, whether it's in the Results panel or a separate window.
#' @param id (optional) The identifier for the browser window to hide
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' cybrowserHide()
#' }
#' @seealso \link{cybrowserShow}
#' @seealso \link{cybrowserDialog}
#' @export
cybrowserHide <- function(id=NULL, base.url = .defaultBaseUrl){
    id.str <- ""
    if(!is.null(id))
        id.str <- paste0(' id="',id,'"')
    
    commandsPOST(paste0('cybrowser hide',id.str),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser List
#'
#' @description List all browsers that are currently open, whether as a dialog or in the results panel.
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of open cybrowser windows
#' @examples \donttest{
#' cybrowserList()
#' }
#' @export
cybrowserList <- function(base.url = .defaultBaseUrl){
    commandsPOST('cybrowser list',
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser Send
#'
#' @description Send the text to the browser indicated by the id and return the 
#' response, if any. Note that the JSON result field could either be a bare string 
#' or JSON formatted text.
#' @param id (optional) The identifier for the browser window
#' @param script (optional) A string that represents a JavaScript variable, script,
#'  or call to be executed in the browser. Note that only string results are returned.
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return String result
#' @examples \donttest{
#' cybrowserSend(id="Window 1", script="navigator.userAgent;")
#' }
#' @export
cybrowserSend <- function(id=NULL, script, base.url = .defaultBaseUrl){
    id.str <- ""
    if(!is.null(id))
        id.str <- paste0(' id="',id,'"')
    
    commandsPOST(paste0('cybrowser send',id.str,' script="',script,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser Show
#'
#' @description Launch Cytoscape's internal web browser in a pane in the Result 
#' Panel. Provide an id for the window if you want subsequent control of the 
#' window via cybrowser hide.
#' @param id (optional) The identifier for the browser window
#' @param text (optional) HTML text to initially load into the browser
#' @param title (optional) Text to be shown in the title bar of the browser window 
#' @param url (optional) The URL the browser should load
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' cybrowserShow(url='http://cytoscape.org')
#' }
#' @seealso \link{cybrowserDialog}
#' @seealso \link{cybrowserHide}
#' @export
cybrowserShow <- function(id=NULL, text=NULL, title=NULL, url=NULL, 
                          base.url = .defaultBaseUrl){
    id.str <- ""
    if(!is.null(id))
        id.str <- paste0(' id="',id,'"')
    
    text.str <- ""
    if(!is.null(text))
        text.str <- paste0(' text="',text,'"')
    
    title.str <- ""
    if(!is.null(title))
        title.str <- paste0(' title="',title,'"')
    
    url.str <- ""
    if(!is.null(url))
        url.str <- paste0(' url="',url,'"')
    
    commandsPOST(paste0('cybrowser show',id.str,text.str,title.str,url.str),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Cybrowser Version
#'
#' @description Display the version of the CyBrowser app.
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Version number
#' @examples \donttest{
#' cybrowserVersion()
#' }
#' @export
cybrowserVersion <- function(base.url = .defaultBaseUrl){
    commandsPOST('cybrowser version',
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Diffusion Basic
#'
#' @description Diffusion will send the selected network view and its selected nodes 
#' to a web-based REST service to calculate network propagation. Results are returned 
#' and represented by columns in the node table.
#' @details Columns are created for each execution of Diffusion and their names 
#' are returned in the response. The nodes you would like to use as input should 
#' be selected. This will be used to generate the contents of the diffusion_input 
#' column, which represents the query vector and corresponds to h in the diffusion
#'  equation.
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Version number
#' @examples \donttest{
#' diffusionBasic()
#' }
#' @export
diffusionBasic <- function(base.url = .defaultBaseUrl){
    commandsPOST('diffusion diffuse',
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Diffusion Advanced
#'
#' @description Diffusion will send the selected network view and its selected nodes 
#' to a web-based REST service to calculate network propagation. Results are returned 
#' and represented by columns in the node table. Advanced operation supports
#' parameters.
#' @details Columns are created for each execution of Diffusion and their names 
#' are returned in the response. The nodes you would like to use as input should 
#' be selected. This will be used to generate the contents of the diffusion_input 
#' column, which represents the query vector and corresponds to h in the diffusion
#' equation.
#' @param heat.column.name (optional) A node column name intended to override the 
#' default table column 'diffusion_input'. This represents the query vector and 
#' corresponds to h in the diffusion equation.
#' @param time (optional) The extent of spread over the network. This corresponds 
#' to t in the diffusion equation.
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Version number
#' @examples \donttest{
#' diffusionAdvanced()
#' }
#' @export
diffusionAdvanced <- function(heat.column.name=NULL, time=NULL, base.url = .defaultBaseUrl){
    heat.str <- ""
    if(!is.null(heat.column.name))
        heat.str <- paste0(' heatColumnName="',heat.column.name,'"')
    
    time.str <- ""
    if(!is.null(time))
        time.str <- paste0(' time="',time,'"')
    
    commandsPOST(paste0('diffusion diffuse_advanced',heat.str,time.str),
                 base.url = base.url)
}
