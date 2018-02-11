# ------------------------------------------------------------------------------
#' Save Session to File
#'
#' @description Saves the current Cytoscape session as a CYS file. 
#' @details If no \code{filename} is provided, then it attempts to
#' save to an existing CYS file associated with the session. If 
#' \code{filename} already exists, then it is overwritten.
#' @param filename (char) name of the session file to save
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' saveSession('myFirstSession')
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

#' Open Session File or URL
#'
#' @description Open a session file or URL. This will clear all networks, tables
#' and styles associated with current session. Be sure to \link{saveSession} first.
#' @param file.location File path or URL (with 'http' prefix)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' openSession()
#' }
#' @seealso saveSession
#' @export
openSession<-function(file.location, base.url=.defaultBaseUrl){
    type = 'file'
    if(startsWith(file.location,'http'))
        type = 'url'
    commandsPOST(paste0('session open ',type,'="',file.location,'"'),base.url=base.url)
}

#' @export
closeSession<-function(save.before.closing, filename=NULL, base.url=.defaultBaseUrl){
    if(save.before.closing)
        saveSession(filename, base.url = base.url)
    commandsPOST('session new',base.url=base.url)
}

