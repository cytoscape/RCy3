# ------------------------------------------------------------------------------
#' @title Save Session to File
#'
#' @description Saves the current Cytoscape session as a CYS file. 
#' @details If no \code{filename} is provided, then it attempts to
#' save to an existing CYS file associated with the session. If 
#' \code{filename} already exists, then it is overwritten.
#' @param filename The path and name of the session file to save
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return server response
#' @examples
#' \donttest{
#' saveSession('/fullpath/mySession')
#' saveSession() 
#' }
#' @export
saveSession<-function(filename=NULL, base.url=.defaultBaseUrl){
    if(is.null(filename)){
        filename <- unname(cyrestGET('session/name', base.url=base.url))
        if(filename=="")
            stop('Save not completed. Provide a filename the first time you save a session.')
    }
    commandsPOST(paste0('session save file="',filename,'"'),base.url=base.url)
}

#' @title Open Session File or URL
#'
#' @description Open a session file or URL. This will clear all networks, tables
#' and styles associated with current session. Be sure to \link{saveSession} first.
#' @param file.location File path or URL (with 'http' prefix)
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' openSession('/fullpath/mySession.CYS')
#' }
#' @seealso saveSession
#' @export
openSession<-function(file.location, base.url=.defaultBaseUrl){
    type = 'file'
    if(startsWith(file.location,'http'))
        type = 'url'
    commandsPOST(paste0('session open ',type,'="',file.location,'"'),base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Close Session
#'
#' @description FUNCTION_DESCRIPTION
#' @param save.before.closing \code{boolean} Whether to save before closing the
#' current session.
#' @param filename (optional) If \code{save.before.closing} is TRUE and the 
#' session has not previously been saved, then the path and name of the session 
#' file to save should be provided. Default is NULL.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' closeSession(FALSE)
#' closeSession(TRUE, '/fullpath/mySession')
#' closeSession(TRUE)
#' }
#' @export
closeSession<-function(save.before.closing, filename=NULL, base.url=.defaultBaseUrl){
    if(save.before.closing)
        saveSession(filename, base.url = base.url)
    
    commandsPOST('session new',base.url=base.url)
}

