# ==============================================================================
# Functions for managing Cytoscape SESSIONS, including save, open and close.
# 
# ------------------------------------------------------------------------------
#' @title Close Session
#'
#' @description Closes the current session in Cytoscape, destroying all unsaved
#' work. 
#' @details A boolean for whether to save before closing is required since you
#' could lose data by closing without saving.
#' @param save.before.closing \code{boolean} Whether to save before closing the
#' current session. If FALSE, then all unsaved work will be lost.
#' @param filename (optional) If \code{save.before.closing} is TRUE and the 
#' session has not previously been saved, then the path and name of the session 
#' file to save should be provided. Default is NULL.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
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

#' @title Open Session File or URL
#'
#' @description Open a session file or URL. This will clear all networks, tables
#' and styles associated with current session. Be sure to \link{saveSession} first.
#' @param file.location File path or URL (with 'http' or 'https' prefix). Default 
#' is a sample session file.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' openSession('/fullpath/mySession.CYS')
#' }
#' @seealso saveSession
#' @export
openSession<-function(file.location=NULL, base.url=.defaultBaseUrl){
    type = 'file'
    if(is.null(file.location))
        file.location <- './sampleData/sessions/Yeast Perturbation.cys'
    else if(startsWith(file.location,'http'))
        type = 'url'
    else if(!isAbsolutePath(file.location))
        file.location = getAbsSandboxPath(file.location)
        
    message(sprintf("Opening %s...",file.location))
    commandsPOST(paste0('session open ',type,'="',file.location,'"'),base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Save Session to File
#'
#' @description Saves the current Cytoscape session as a CYS file. 
#' @details If no \code{filename} is provided, then it attempts to
#' save to an existing CYS file associated with the session. If 
#' \code{filename} already exists, then it is overwritten.
#' @param filename Full path or path relavtive to current working directory, 
#' in addition to the name of the file. The \code{.cys} extension is 
#' automatically added. Leave blank to update previously saved session file. 
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @param overwriteFile (optional) FALSE allows an error to be generated if the file already exists;
#' TRUE allows Cytoscape to overwrite it without asking. Default value is TRUE.
#' @return server response
#' @details Unlike most export functions in RCy3, Cytoscape will automatically
#' overwrite CYS session files with the same name. You will not be prompted to
#' confirm or reject overwrite. Use carefully!
#' @examples
#' \donttest{
#' saveSession('/fullpath/mySession')
#' saveSession() 
#' }
#' @importFrom R.utils isAbsolutePath
#' @import glue
#' @export
saveSession<-function(filename=NULL, base.url=.defaultBaseUrl, overwriteFile=TRUE){
    if(is.null(filename)){
        filename <- unname(cyrestGET('session/name', base.url=base.url))
        if(filename=="")
            stop('Save not completed. Provide a filename the first time you save a session.')
        commandsPOST(paste0('session save'),base.url=base.url)
    } else {
        ext <- ".cys$"
        if (!grepl(ext,filename))
            filename <- paste0(filename,".cys")
        if(!isAbsolutePath(filename))
            filename <- getAbsSandboxPath(filename)
        if (file.exists(filename))
            warning("This file has been overwritten.",
                    call. = FALSE,
                    immediate. = TRUE)
        commandsPOST(paste0('session save as file="',
                            filename,'"'), 
                     base.url=base.url)
    }
}
