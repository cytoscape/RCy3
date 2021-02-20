# ==============================================================================
# Functions for constructing any arbitrary CyREST API or Commands API method via
# standard GET, PUT, POST and DELETE protocols. These functions handle marshalling
# and unmarshalling of urls, parameters and returns so that higher-level functions
# can work with R-friendly arguments and returns.
#
# I. CyREST API functions
# II. Commands API functions
# III. Internal functions 
# 
# Note: This is where the bulk of the dependencies for other packages are used,
# e.g., utils, httr, RJSONIO, etc. Follow the use of @importFrom where prudent.
#
# ==============================================================================
# I. CyREST API functions
# ------------------------------------------------------------------------------
#' Open Swagger docs for CyREST API 
#'
#' @description Opens swagger docs in default browser for a live
#' instance of CyREST operations.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Web page in browser
#' @export
#' @examples
#' \donttest{
#' cyrestAPI()
#' }
#' @importFrom utils browseURL

cyrestAPI<-function(base.url=.defaultBaseUrl){
    browseURL(paste(base.url,'/swaggerUI/swagger-ui/index.html?url=',base.url,'/swagger.json#/',sep=""))
}

# ------------------------------------------------------------------------------
#' @title CyREST DELETE
#'
#' @description Constructs the query, makes DELETE call and processes the result
#' @param operation A string to be converted to the REST query namespace
#' @param parameters A named list of values to be converted to REST query parameters 
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return CyREST result content
#' @examples \donttest{
#' cyrestDELETE()
#' }
#' @importFrom RJSONIO fromJSON isValidJSON
#' @importFrom httr DELETE
#' @importFrom utils URLencode
#' @export
cyrestDELETE <- function(operation=NULL, parameters=NULL, base.url=.defaultBaseUrl){
    if(!findRemoteCytoscape()){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    tryCatch(
        res <- DELETE(url=URLencode(q.url)), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    if(length(res$content)>0){
        res.char <- rawToChar(res$content)
        if (isValidJSON(res.char, asText = TRUE)){
            return(fromJSON(res.char))
        } else {
            return(res.char)
        }
        invisible(res)
    }
    } else {
        q.url <- paste('http://127.0.0.1:1234/v1', .pathURLencode(operation), sep="/")
        if(!is.null(parameters)){
            q.params <- .prepGetQueryArgs(parameters)
            q.url <- paste(q.url, q.params, sep="?")
        }
        res <- doRequestRemote("DELETE", URLencode(q.url))
        if(length(res$content)>0){
            res.char <- rawToChar(res$content)
            if (isValidJSON(res.char, asText = TRUE)){
                return(fromJSON(res.char))
            } else {
                return(res.char)
            }
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST GET
#'
#' @description Constructs the query, makes GET call and processes the result
#' @param operation A string to be converted to the REST query namespace
#' @param parameters A named list of values to be converted to REST query parameters 
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return CyREST result content
#' @examples \donttest{
#' cyrestGET()
#' }
#' @importFrom RJSONIO fromJSON isValidJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @export
cyrestGET <- function(operation=NULL, parameters=NULL, base.url=.defaultBaseUrl){
    if(!findRemoteCytoscape()){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    tryCatch(
        res <- GET(url=URLencode(q.url)), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    if(length(res$content)>0){
        res.char <- rawToChar(res$content)
        if (isValidJSON(res.char, asText = TRUE)){
            return(fromJSON(res.char))
        } else {
            return(res.char)
        }
    } else{
        invisible(res)
    }
    } else {
        q.url <- paste('http://127.0.0.1:1234/v1', .pathURLencode(operation), sep="/")
        if(!is.null(parameters)){
            q.params <- .prepGetQueryArgs(parameters)
            q.url <- paste(q.url, q.params, sep="?")
        }
        res <- doRequestRemote("GET", URLencode(q.url))
        if(length(res$content)>0){
            res.char <- rawToChar(res$content)
            if (isValidJSON(res.char, asText = TRUE)){
                return(fromJSON(fromJSON(res.char)$text))
            } else {
                return(res.char)
            }
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST POST
#'
#' @description Constructs the query and body, makes POST call and processes the result
#' @param operation A string to be converted to the REST query namespace
#' @param parameters A named list of values to be converted to REST query parameters 
#' @param body A named list of values to be converted to JSON
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return CyREST result content
#' @examples \donttest{
#' cyrestPOST()
#' }
#' @importFrom RJSONIO fromJSON toJSON isValidJSON
#' @importFrom httr POST content_type_json
#' @importFrom utils URLencode
#' @export
cyrestPOST <- function(operation, parameters=NULL, body=NULL, base.url=.defaultBaseUrl){
    if(!findRemoteCytoscape()){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    q.body <- toJSON(body)
    tryCatch(
        res <- POST(url=URLencode(q.url), body=q.body, encode="json", content_type_json()), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    if(length(res$content)>0){
        res.char <- rawToChar(res$content)
        if (isValidJSON(res.char, asText = TRUE)){
            return(fromJSON(res.char))
        } else {
            return(res.char)
        }
        invisible(res)
    }
    } else {
        q.url <- paste('http://127.0.0.1:1234/v1', .pathURLencode(operation), sep="/")
        if(!is.null(parameters)){
            q.params <- .prepGetQueryArgs(parameters)
            q.url <- paste(q.url, q.params, sep="?")
        }
        q.body <- body
        res <- doRequestRemote("POST", URLencode(q.url), q.body, headers=list("Content-Type" = "application/json", "Accept" = "application/json"))
        if(length(res$content)>0){
            res.char <- rawToChar(res$content)
            if (isValidJSON(res.char, asText = TRUE)){
                return(fromJSON(res.char))
            } else {
                return(res.char)
            }
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST PUT
#'
#' @description Constructs the query and body, makes PUT call and processes the result
#' @param operation A string to be converted to the REST query namespace
#' @param parameters A named list of values to be converted to REST query parameters 
#' @param body A named list of values to be converted to JSON
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return CyREST result content
#' @examples \donttest{
#' cyrestPUT()
#' }
#' @importFrom RJSONIO fromJSON isValidJSON toJSON
#' @importFrom httr PUT content_type_json
#' @importFrom utils URLencode
#' @export
cyrestPUT <- function(operation, parameters=NULL, body=FALSE, base.url=.defaultBaseUrl){
    if(!findRemoteCytoscape()){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    q.body <- toJSON(body)
    tryCatch(
        res <- PUT(url=URLencode(q.url), body=q.body, encode="json", content_type_json()), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    if(length(res$content)>0){
        res.char <- rawToChar(res$content)
        if (isValidJSON(res.char, asText = TRUE)){
            return(fromJSON(res.char))
        } else {
            return(res.char)
        }
        invisible(res)
    }
    } else {
        q.url <- paste('http://127.0.0.1:1234/v1', .pathURLencode(operation), sep="/")
        if(!is.null(parameters)){
            q.params <- .prepGetQueryArgs(parameters)
            q.url <- paste(q.url, q.params, sep="?")
        }
        q.body <- body
        res <- doRequestRemote("PUT", URLencode(q.url), q.body, headers=list("Content-Type" = "application/json", "Accept" = "application/json"))
        if(length(res$content)>0){
            res.char <- rawToChar(res$content)
            if (isValidJSON(res.char, asText = TRUE)){
                return(fromJSON(res.char))
            } else {
                return(res.char)
            }
        } else{
            invisible(res)
        }
    }
}

# ==============================================================================
# II. Commands API functions
# ------------------------------------------------------------------------------
#' Open Swagger docs for CyREST Commands API 
#'
#' @description Opens swagger docs in default browser for a live
#' instance of Commands available via CyREST.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Web page in browser
#' @export
#' @examples
#' \donttest{
#' commandsAPI()
#' }
#' @importFrom utils browseURL

commandsAPI<-function(base.url=.defaultBaseUrl){
    browseURL(paste(base.url,'/swaggerUI/swagger-ui/index.html?url=',base.url,'/commands/swagger.json#/',sep=""))
}

# ------------------------------------------------------------------------------
#' @title Commands GET
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST query URL, executes a GET
#' request, and parses the result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list}, \code{status} or None.
#' @examples
#' \donttest{
#' commandsGET('layout get preferred network=current')
#' commandsGET('network list properties network=current')
#' commandsGET('layout force-directed defaultNodeMass=1')
#' }
#' @importFrom XML htmlParse xmlValue xpathSApply
#' @importFrom httr GET
#' @export
commandsGET<-function(cmd.string, base.url = .defaultBaseUrl){
    if(!findRemoteCytoscape()){
    q.url <- .command2getQuery(cmd.string,base.url)
    tryCatch(
        res <- GET(q.url), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    res.html = htmlParse(rawToChar(res$content), asText=TRUE)
    res.elem = xpathSApply(res.html, "//p", xmlValue)
    if(startsWith(res.elem[1],"[")){
        res.elem[1] = gsub("\\[|\\]|\"","",res.elem[1])
        res.elem2 = unlist(strsplit(res.elem[1],"\n"))[1]
        res.list = unlist(strsplit(res.elem2,","))
    }else {
        res.list = unlist(strsplit(res.elem[1],"\n\\s*"))
        res.list = res.list[!(res.list=="Finished")]
    }
    if(length(res.list)>0){
        res.list
    } else {
        invisible(res.list)
    }
    } else {
        q.url <- .command2getQuery(cmd.string, 'http://127.0.0.1:1234/v1')
        res <- doRequestRemote("GET", URLencode(q.url), headers=list("Accept" = "text/plain"))
        res.html = htmlParse(rawToChar(res$content), asText=TRUE)
        res.elem = xpathSApply(res.html, "//p", xmlValue)
        if(startsWith(res.elem[1],"[")){
            res.elem[1] = gsub("\\[|\\]|\"","",res.elem[1])
            res.elem2 = unlist(strsplit(res.elem[1],"\n"))[1]
            res.list = unlist(strsplit(res.elem2,","))
        }else {
            res.list = unlist(strsplit(res.elem[1],"\n\\s*"))
            res.list = res.list[!(res.list=="Finished")]
        }
        if(length(res.list)>0){
            fromJSON(res.list)$text
        } else {
            invisible(res.list)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title Commands Help
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function returns a list of available commands or args.
#' @details Works with or without 'help' command prefix. Note that if you ask about a command that doesn't
#' have any arguments, this function will run the command!
#' @param cmd.string (char) command
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of available commands or args
#' @examples
#' \donttest{
#' commandsHelp()
#' commandsHelp('node')
#' commandsHelp('node get attribute')
#' }
#' @importFrom XML htmlParse xmlValue xpathSApply
#' @importFrom httr GET
#' @importFrom utils head tail
#' @export
commandsHelp<-function(cmd.string='help', base.url = .defaultBaseUrl){
    s=sub('help *','',cmd.string)
    if(!findRemoteCytoscape()){
    q.url <- .command2getQuery(s,base.url)
    tryCatch(
        res <- GET(q.url), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    res.html = htmlParse(rawToChar(res$content), asText=TRUE)
    res.elem = xpathSApply(res.html, "//p", xmlValue)
    res.list = res.elem
    if (length(res.elem)==1){
        res.list = unlist(strsplit(res.elem[1],"\n\\s*"))
    }
    print(head(res.list,1))
    vapply(tail(res.list,-1), trimws, character(1), USE.NAMES = FALSE)
    } else {
        q.url <- .command2getQuery(s, 'http://127.0.0.1:1234/v1')
        res <- doRequestRemote("GET", URLencode(q.url), headers=list("Accept" = "text/plain"))
        res.html = htmlParse(rawToChar(res$content), asText=TRUE)
        res.elem = xpathSApply(res.html, "//p", xmlValue)
        res.list = res.elem
        if (length(res.elem)==1){
            res.list = fromJSON(unlist(strsplit(res.elem[1],"\n\\s*")))
        }
        print(head(res.list,1))
        vapply(tail(res.list,-1), trimws, character(1), USE.NAMES = FALSE)
    }
}

# ------------------------------------------------------------------------------
#' @title Commands POST
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST query URL, executes a
#' POST request, and parses the result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list}, \code{named list}, \code{status} or None.
#' @examples
#' \donttest{
#' commandsPOST('layout get preferred')
#' commandsPOST('network list properties')
#' commandsPOST('layout force-directed defaultNodeMass=1')
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom httr POST content_type_json
#' @export
commandsPOST<-function(cmd.string, base.url = .defaultBaseUrl){
    if(!findRemoteCytoscape()){
    post.url = .command2postQueryUrl(cmd.string,base.url)
    post.body = .command2postQueryBody(cmd.string)
    tryCatch(
        res <- POST(url=post.url, body=post.body, encode="json", content_type_json()), 
        error=function(c) .cyError(c, res),
        warnings=function(c) .cyWarnings(c, res),
        finally=.cyFinally(res)
    )
    if(length(res$content)>0){
        res.data = fromJSON(rawToChar(res$content))$data
        if(length(res.data)>0){
            return(res.data)
        } else{
            invisible(res.data)
        }
    }else {
        invisible(res)
    }
    } else {
        post.url = .command2postQueryUrl(cmd.string, 'http://127.0.0.1:1234/v1')
        post.body = .command2postQueryBody(cmd.string)
        res <- doRequestRemote("POST", URLencode(post.url), post.body, headers=list("Content-Type" = "application/json", "Accept" = "application/json"))
        print(names(res))
        return(rawToChar(res$content))
    }
}
# ------------------------------------------------------------------------------
#' @title Run a Command
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST query URL, executes a GET
#' request, and parses the result content into an R list object. Same as commandsGET.
#' @param cmd.string (char) command
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return A \code{list}, \code{status} or None.
#' @examples
#' \donttest{
#' commandsRun('layout get preferred')
#' commandsRun('network list properties')
#' commandsRun('layout force-directed defaultNodeMass=1')
#' }
#' @export
commandsRun<-function(cmd.string, base.url = .defaultBaseUrl){
    commandsGET(cmd.string,base.url)
}

# ------------------------------------------------------------------------------
#' @title Command Echo
#'
#' @description The echo command will display the value of the variable specified 
#' by the variableName argument, or all variables if variableName is not provided.
#' @param variable.name (optional) The name of the variable to display. Default is 
#' to display all variable values using "*".
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Value of variable
#' @examples \donttest{
#' commandEcho()
#' }
#' @export
commandEcho <- function(variable.name="*", base.url = .defaultBaseUrl){
    commandsPOST(paste0('command echo variableName="',variable.name,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Command Open Dialog
#'
#' @description The command line dialog provides a field to enter commands and 
#' view results. It also provides the help command to display namespaces, 
#' commands, and arguments
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' commandOpenDialog()
#' }
#' @export
commandOpenDialog <- function(base.url = .defaultBaseUrl){
    commandsPOST('command open dialog',
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Command Pause
#'
#' @description The pause command displays a dialog with the text provided in the 
#' message argument and waits for the user to click OK.
#' @param message (optional) Text to display in pause dialog
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' commandPause('Please click OK to continue.')
#' }
#' @export
commandPause <- function(message="", base.url = .defaultBaseUrl){
    commandsPOST(paste0('command pause message="',message,'"'),
                 base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Command Quit
#'
#' @description This command causes Cytoscape to exit. It is typically used at 
#' the end of a script file
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' commandQuit()
#' }
#' @export
commandQuit <- function(base.url = .defaultBaseUrl){
    commandsPOST('command quit',
                 base.url = base.url)
}
# ------------------------------------------------------------------------------
#' @title Command Run File
#'
#' @description The run command will execute a command script from the file pointed 
#' to by the file argument, which should contain Cytoscape commands, one per line. 
#' Arguments to the script are provided by the args argument
#' @param file Path to command script file
#' @param args The script arguments as key:value pairs separated by commas
#' @param base.url (optional) Ignore unless you need to specify a custom domain, 
#' port or version to connect to the CyREST API. Default is http://localhost:1234 
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' commandRunFile('/path/to/my/file.txt')
#' }
#' @export
commandRunFile <- function(file, args=NULL, base.url = .defaultBaseUrl){
    args.str <- ""
    if(!is.null(args))
        args.str <- paste0(' args="',args,'"')
    
    commandsPOST(paste0('command run',args.str,' file="',file,'"'),
                 base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Command Sleep
#'
#' @description The sleep command will pause processing for a period of time as 
#' specified by duration seconds. It is typically used as part of a command script.
#' @param duration (optional) The time in seconds to sleep
#' @param base.url (optional) Ignore unless you need to specify a custom domain, #' port or version to connect to the CyREST API. Default is http://localhost:1234 #' and the latest version of the CyREST API supported by this version of RCy3.
#' @return None
#' @examples \donttest{
#' commandSleep(5)
#' }
#' @export
commandSleep <- function(duration=NULL, base.url = .defaultBaseUrl){
    dur.str <- ""
    if(!is.null(duration))
        dur.str <- paste0(' duration="',duration,'"')
    
    commandsPOST(paste0('command sleep',dur.str),
                 base.url = base.url)
}

# ==============================================================================
# III. Internal functions
# 
# Dev Notes: Prefix internal functions with a '.'. Do not @export and in general
# skip royxgen docs for these functions, with the exception of @importFrom lines.
# ------------------------------------------------------------------------------
# Command string to CyREST GET query URL
#
# @description Converts a command string to a CyREST GET query URL.
# @param cmd.string (char) command
# @param base.url cyrest base url for communicating with cytoscape
# @return CyREST GET URL
# @examples
# \donttest{
# .command2getQuery('layout get preferred')
# }
#' @importFrom utils URLencode
.command2getQuery<-function(cmd.string, base.url = .defaultBaseUrl){
    cmd.string = sub(" ([A-Za-z0-9_-]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    cmd = cmdargs[1]
    if(is.na(cmd)){cmd=""}
    q.cmd = URLencode(paste(base.url, "commands", sub(" ","/",cmd), sep="/"))
    args = cmdargs[2]
    if (is.na(args)){
        q.cmd
    }else{
        args = gsub("\"","",args)
        p = "[A-Za-z0-9_-]+="
        m = gregexpr(p,args)
        args1 = unlist(regmatches(args,m))
        args1 = gsub('=','',args1)
        #args1 = unlist(str_extract_all(args,"[A-Za-z0-9_-]+(?==)")) # requires stringr lib
        args2 = unlist(strsplit(args," *[A-Za-z0-9_-]+="))
        args2 = args2[-1]
        names(args2) <- args1
        q.args = .prepGetQueryArgs(args2)
        paste(q.cmd,q.args,sep="?")
    }
}

# ------------------------------------------------------------------------------
# Command string to CyREST POST query URL
#
# @description Converts a command string to a CyREST POST query URL.
# @param cmd.string (char) command
# @param base.url cyrest base url for communicating with cytoscape
# @return CyREST POST URL
# @examples
# \donttest{
# .command2postQueryUrl('network clone network="current"')
# # http://localhost:1234/v1/commands/network/clone
# }
#' @importFrom utils URLencode
.command2postQueryUrl<-function(cmd.string, base.url = .defaultBaseUrl){
    cmd.string = sub(" ([A-Za-z0-9_-]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    cmd = cmdargs[1]
    if(is.na(cmd)){cmd=""}
    URLencode(paste(base.url, "commands", sub(" ","/",cmd), sep="/"))
}

# ------------------------------------------------------------------------------
# Command string to CyREST POST query JSON body.
#
# @description Converts a command string to a CyREST POST query JSON body.
# @details POST requests require at leaset one arg, so a "filler" arg is provided if NULL
# @param cmd.string (char) command
# @param base.url cyrest base url for communicating with cytoscape
# @return CyREST POST JSON body
# @examples
# \donttest{
# .command2postQueryBody('network clone network="current"')
# # {
# # "network": "current"
# # }
# }
#' @importFrom RJSONIO toJSON
.command2postQueryBody<-function(cmd.string){
    cmd.string = sub(" ([A-Za-z0-9_-]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    args = cmdargs[2]
    
    if (is.na(args))
        args = 'atLeastOneArg=required' #supply a benign "filler" if NULL
    
    args = gsub("\"","",args)
    p = "[A-Za-z0-9_-]+="
    m = gregexpr(p,args)
    args1 = unlist(regmatches(args,m))
    args1 = gsub('=','',args1)
    #args1 = unlist(str_extract_all(args,"[A-Za-z0-9_-]+(?==)")) # requires stringr lib
    args2 = unlist(strsplit(args," *[A-Za-z0-9_-]+="))
    args2 = args2[-1]
    names(args2) <- args1
    return(args2)
}
# Takes a named list and makes a string for GET query urls
#' @importFrom utils URLencode
.prepGetQueryArgs <- function(named.args){
    args1 <- names(named.args)
    args2 <- unlist(unname(named.args))
    q.args = paste(args1[1],URLencode(as.character(args2[1])),sep="=")
    for (i in seq(args1)[-1]){
        arg = paste(args1[i],URLencode(as.character(args2[i])),sep="=")
        q.args = paste(q.args,arg,sep="&")
    }
    return(q.args)
}

# Parses all the possible list types and keywords accepted by Commands API.
# If column designation is supported, simply provide a column name; otherwise
# it is assumed to not be supported and returns a simple list. 
.prepPostQueryLists <- function(cmd.list=NULL, cmd.by.col=NULL){
    if (is.null(cmd.list)) {
        cmd.list.ready = "selected" #need something here for edge selections to work
    } else if (!is.null(cmd.by.col)) {
        cmd.list.col = NULL
        for (i in seq_len(length(cmd.list))) {
            cmd.list.col[i] = paste(cmd.by.col, cmd.list[i], sep = ":")
        }
        cmd.list.ready = paste(cmd.list.col, collapse = ",")
    } else {
        cmd.list.ready = paste(cmd.list, collapse=",")
    }
    return(cmd.list.ready)
}
# ==============================================================================
# IV. Jupyter-bridge 
# ------------------------------------------------------------------------------
#' @title findRemoteCytoscape
#' @description findRemoteCytoscapeL
#' @examples
#' \donttest{
#' findRemoteCytoscape()
#' }
#' @export
findRemoteCytoscape<-function(){
    checkNotebookIsRunning()
    checkRunningRemote()
    if(is.null(checkRunningRemote())){
        stop('Cannot find local or remote Cytoscape. Start Cytoscape and then proceed.')
    }
    return(runningRemoteCheck())
}
# ------------------------------------------------------------------------------
# CyRest Message Handler
#
# @description Provides helpful messaages for CyREST issues.
# @c Error condition
# @res Result with error
# @return A stop message
# @examples
# \donttest{
# tryCatch(
#     res <- cyRestGET("version"),
#     error=function(c) {.cyError(c, res)}
# )
#

.cyError<-function(c, res){
    err_conn = 'Connection refused' # Connection Error
    if (length(grep(err_conn,c$message)) == 0){ # Certain 404 Errors
        stop(simpleError("Not Found"))
    } else {
        message("Oh no! I can't find Cytoscape. RCy3 can not continue!
Please check that Cytoscape is running, CyREST is installed and your base.url parameter is correct.")
        write(sprintf("Failed to execute: %s",res[[1]]), stderr())
        stop(simpleError(conditionMessage(c)))
    } 
}

.cyWarnings<-function(c, res){
    #Pass along any warnings and carry on
    message(c$message)
}

#' @importFrom RJSONIO fromJSON
#' @importFrom XML htmlParse xmlValue xpathSApply
.cyFinally<-function(res){
    if(!is.null(res)){
        
        # Check HTTP Errors
        if(res$status_code > 299){
            write(sprintf("Failed to execute: %s",res[[1]]), stderr())
            if(res[[3]]$`content-type` == "text/plain" ||
               res[[3]]$`content-type` == "text/html;charset=iso-8859-1"){
                errmsg <- paste(xpathSApply(htmlParse(rawToChar(res$content), asText=TRUE), "//p", xmlValue),
                                xpathSApply(htmlParse(rawToChar(res$content), asText=TRUE), "//pre", xmlValue))
                stop(simpleError(errmsg))
            } else if (res[[3]]$`content-type` == "application/json"){
                stop(simpleError(fromJSON(rawToChar(res$content))$errors[[1]]$message))
            } else {
                stop()
            }
        }
    }
}

# ------------------------------------------------------------------------------
# Encode Operation
#
# Applies URLencode to each step along the URL path without clobbering the path.
.pathURLencode <- function(operation){
    steps <- strsplit(operation,"\\/")[[1]]
    paste(lapply(steps, URLencode, reserved = TRUE), collapse = "/")
}
    