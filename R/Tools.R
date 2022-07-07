# ==============================================================================
# Functions related to TOOLS found in the Tools Menu in Cytoscape.
# 
# ------------------------------------------------------------------------------
#' @title Analyze Network
#'
#' @description Calculate various network statistics.
#' @param directed (optional) If TRUE, the network is considered a directed 
#' graph. Default is FALSE.
#' @param base.url (optional) Ignore unless you need to specify a custom 
#' domain, port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported 
#' by this version of RCy3.
#' @return Named list of summary statistics
#' @details The results are added to the Node and Edge tables and the Results 
#' Panel. The summary statistics in the Results Panel are also returned by 
#' the function as a list of named values.
#' @examples \donttest{
#' analyzeNetwork()
#' analyzeNetwork(TRUE)
#' }
#' @export
analyzeNetwork <- function(directed = FALSE, base.url = .defaultBaseUrl){
    commandsPOST(paste0('analyzer analyze directed=',directed),
                 base.url = base.url)
    
}

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

# ------------------------------------------------------------------------------
#' @title Merge Networks
#' 
#' @description Combine networks via union, intersection, or difference 
#' operations. Lots of optional parameters choose from!
#' @param sources List of network names to be merged.
#' @param title (optional) Title of the resulting merged network. Default is a
#' concatentation of operation and source network titles.
#' @param operation (optional) Type of merge: union (default), intersection or
#' difference.
#' @param nodeKeys (optional) An order-dependent list of columns to match  
#' nodes across source networks. Default is "name" column for all sources.
#' @param nodeMergeMap (optional) A list of column merge records specifying
#' how to merge node table data. Each record should be of the form: 
#' c("network1 column", "network2 column", "merged column", "type"), where
#' column names are provided and type is String, Integer, Double or List.
#' @param nodesOnly (optional) If TRUE, this will merge the node tables and 
#' ignore edge and network table data. Default is FALSE.
#' @param edgeKeys (optional) An order-dependent list of columns to match 
#' edges across source networks. Default is "name" column for all sources.
#' @param edgeMergeMap (optional) A list of column merge records specifying
#' how to merge edge table data. Each record should be of the form: 
#' c("network1 column", "network2 column", "merged column", "type"), where
#' column names are provided and type is String, Integer, Double or List.
#' @param networkMergeMap (optional) A list of column merge records specifying
#' how to merge network table data. Each record should be of the form: 
#' c("network1 column", "network2 column", "merged column", "type"), where
#' column names are provided and type is String, Integer, Double or List.
#' @param inNetworkMerge (optional) If TRUE (default), nodes and edges with 
#' matching attributes in the same network will be merged. 
#' @param base.url (optional) Ignore unless you need to specify a custom 
#' domain, port or version to connect to the CyREST API. Default is 
#' http://localhost:1234 and the latest version of the CyREST API supported 
#' by this version of RCy3.
#' @return SUID of resulting merged network
#' @examples \donttest{
#' mergeNetworks(c("Network 1", "Network 2"), "Merged Network")
#' mergeNetworks(c("my network","string network"), "Merged Network", 
#'               nodeKeys=c("HGNC","query term"))
#' }
#' @export
mergeNetworks <- function(sources = NULL,
                          title = NULL,
                          operation = "union",
                          nodeKeys = NULL,
                          nodeMergeMap = NULL,
                          nodesOnly = FALSE,
                          edgeKeys = NULL,
                          edgeMergeMap = NULL,
                          networkMergeMap = NULL,
                          inNetworkMerge = TRUE,
                          base.url = .defaultBaseUrl) {
    cmd.string <- 'network merge' # a good start
    
    # sources must be suppled
    if(is.null(sources)) {
        message("Missing sources!")
        return(NULL)
    } else {
        sources.str <- paste(sources, collapse = ",")
        cmd.string <- paste0(cmd.string,' sources="',sources.str,'"')
    }
    
    # defaults
    cmd.string <- paste0(cmd.string,' operation=',operation)
    cmd.string <- paste0(cmd.string,' nodesOnly=',nodesOnly)
    cmd.string <- paste0(cmd.string,' inNetworkMerge=',inNetworkMerge)
    
    # optional args
    if(!is.null(title))
        cmd.string <- paste0(cmd.string,' netName="',title,'"')
    if(!is.null(nodeKeys))
        cmd.string <- paste0(cmd.string,' nodeKeys="',paste(nodeKeys, collapse = ","),'"')
    if(!is.null(edgeKeys))
        cmd.string <- paste0(cmd.string,' edgeKeys="',paste(edgeKeys, collapse = ","),'"')
    if(!is.null(nodeMergeMap)){
        nodeMergeMap.str <- paste(nodeMergeMap, collapse = ",")
        nodeMergeMap.str <- gsub("c\\(", "{", nodeMergeMap.str)
        nodeMergeMap.str <- gsub("\\)", "}", nodeMergeMap.str)
        cmd.string <- paste0(cmd.string,' nodeMergeMap="',nodeMergeMap.str,'"')
    }
    if(!is.null(edgeMergeMap)){
        edgeMergeMap.str <- paste(edgeMergeMap, collapse = ",")
        edgeMergeMap.str <- gsub("c\\(", "{", edgeMergeMap.str)
        edgeMergeMap.str <- gsub("\\)", "}", edgeMergeMap.str)
        cmd.string <- paste0(cmd.string,' edgeMergeMap="',edgeMergeMap.str,'"')
    }
    if(!is.null(networkMergeMap)){
        networkMergeMap.str <- paste(networkMergeMap, collapse = ",")
        networkMergeMap.str <- gsub("c\\(", "{", networkMergeMap.str)
        networkMergeMap.str <- gsub("\\)", "}", networkMergeMap.str)
        cmd.string <- paste0(cmd.string,' networkMergeMap="',networkMergeMap.str,'"')
    }
    
    res.data <- commandsPOST(cmd.string, base.url = base.url)
    
    if(!is.null(res.data$SUID))
        return(res.data$SUID)
    else
        return(res.data)
}

# ------------------------------------------------------------------------------
#' @title importFileFromUrl
#'
#' @description The source URL identifies a file to be transferred from a cloud resource to either the
#' to the current Cytoscape directory (if executing on the Cytoscape workstation) or sandbox (if
#' executing on a remote server or a sandbox was explicitly created). If the destination file already
#' exists, it is overwritten. The 'destFile' can be an absolute path if the workflow is
#' executing on the local Cytoscape workstation.  
#' Supported URLs include:
#' Raw URL: URL directly references the file to download (e.g., http://tpsoft.com/museum_images/IBM%20PC.JPG
#'                                                           Dropbox: Use the standard Dropbox 'Get Link' feature to create the 'sourceUrl' link in the clipboard (e.g., https://www.dropbox.com/s/r15azh0xb53smu1/GDS112_full.soft?dl=0)
#'                                                           GDrive: Use the standard Google Drive 'Get Link' feature to create the 'sourceUrl' link in the clipboard (e.g., https://drive.google.com/file/d/12sJaKQQbesF10xsrbgiNtUcqCQYY1YI3/view?usp=sharing)
#'                                                           OneDrive: Use the OneDrive web site to right click on the file, choose the 'Embed' menu option, then copy the URL in the iframe's ``src`` parameter into the clipboard (e.g., https://onedrive.live.com/embed?cid=C357475E90DD89C4&resid=C357475E90DD89C4%217207&authkey=ACEU5LrVtI_jWTU)
#'        GitHub: Use the GitHub web site to show the file or a link to it, and capture the URL in the clipboard (e.g., https://github.com/cytoscape/file-transfer-app/blob/master/test_data/GDS112_full.soft)
#' Note that GitHub enforces a limit on the size of a file that can be stored there. We advise that you take this
#' into account when choosing a cloud service for your files.
#' When you capture a URL in the clipboard, you should copy it into your program for use with this function.
#' This function is most useful for Notebooks running on the local Cytoscape workstation. For Notebooks
#' that could run on a remote server, consider using sandboxUrlTo() and related sandbox functions.                                                                                  
#' @param sourceURL URL addressing cloud file to download
#' @param destFile Name of file in the R workflow's file system ... if None, use file name in source_file
#' @param overwrite Name of sandbox containing file. None means "the current sandbox".
#' @param base.url Ignore unless you need to specify a custom domain, port or version to connect to the CyREST API. Default is http://127.0.0.1:1234 and the latest version of the CyREST API supported by this version of RCy3.
#' @return dict: {'filePath': <new file's absolute path in Cytoscape workstation>, 'fileByteCount': number of bytes read}
#' @examples \donttest{
#' importFileFromUrl()
#' }
#' @importFrom glue glue
#' @import RCurl
#' @export
importFileFromUrl <- function(sourceURL, destFile, overwrite=TRUE, base.url=.defaultBaseUrl){
    return(sandboxUrlTo(sourceURL, destFile, overwrite=overwrite, sandboxName=NULL, base.url=base.url))
}
