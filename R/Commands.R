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
#' @title CyREST GET
#'
#' @description FUNCTION_DESCRIPTION
#' @param operation DESCRIPTION
#' @param parameters DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' cyrestGET()
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @export
cyrestGET <- function(operation=NULL, parameters=NULL, base.url=.defaultBaseUrl){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    res <- GET(url=URLencode(q.url))
    if(res$status_code > 299){
        write(sprintf("RCy3::cyrestGET, HTTP Error Code: %d\n url=%s", 
                      res$status_code, URLencode(q.url)), stderr())
        stop()
    } else {
        if(length(res$content)>0){
            return(fromJSON(rawToChar(res$content)))
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST POST
#'
#' @description FUNCTION_DESCRIPTION
#' @param operation DESCRIPTION
#' @param parameters DESCRIPTION
#' @param body DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' cyrestPOST()
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom RJSONIO toJSON
#' @importFrom httr POST
#' @importFrom httr content_type_json
#' @importFrom utils URLencode
#' @export
cyrestPOST <- function(operation, parameters=NULL, body=NULL, base.url=.defaultBaseUrl){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    q.body <- toJSON(body)
    res <- POST(url=URLencode(q.url), body=q.body, encode="json", content_type_json())
    if(res$status_code > 299){
        write(sprintf("RCy3::cyrestPOST, HTTP Error Code: %d\n url=%s\n body=%s", 
                      res$status_code, URLencode(q.url), q.body), stderr())
        stop()
    } else {
        if(length(res$content)>0){
            return(fromJSON(rawToChar(res$content)))
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST PUT
#'
#' @description FUNCTION_DESCRIPTION
#' @param operation DESCRIPTION
#' @param parameters DESCRIPTION
#' @param body DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' cyrestPUT()
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom RJSONIO toJSON
#' @importFrom httr PUT
#' @importFrom httr content_type_json
#' @importFrom utils URLencode
#' @export
cyrestPUT <- function(operation, parameters=NULL, body=FALSE, base.url=.defaultBaseUrl){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    q.body <- toJSON(body)
    res <- PUT(url=URLencode(q.url), body=q.body, encode="json", content_type_json())
    if(res$status_code > 299){
        write(sprintf("RCy3::cyrestPUT, HTTP Error Code: %d\n url=%s\n body=%s", 
                      res$status_code, URLencode(q.url), q.body), stderr())
        stop()
    } else {
        if(length(res$content)>0){
            return(fromJSON(rawToChar(res$content)))
        } else{
            invisible(res)
        }
    }
}

# ------------------------------------------------------------------------------
#' @title CyREST DELETE
#'
#' @description FUNCTION_DESCRIPTION
#' @param operation DESCRIPTION
#' @param parameters DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' cyrestDELETE()
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom httr DELETE
#' @importFrom utils URLencode
#' @export
cyrestDELETE <- function(operation=NULL, parameters=NULL, base.url=.defaultBaseUrl){
    q.url <- paste(base.url, operation, sep="/")
    if(!is.null(parameters)){
        q.params <- .prepGetQueryArgs(parameters)
        q.url <- paste(q.url, q.params, sep="?")
    }
    res <- DELETE(url=URLencode(q.url))
    if(res$status_code > 299){
        write(sprintf("RCy3::cyrestGET, HTTP Error Code: %d\n url=%s", 
                      res$status_code, URLencode(q.url)), stderr())
        stop()
    } else {
        if(length(res$content)>0){
            return(fromJSON(rawToChar(res$content)))
        } else{
            invisible(res)
        }
    }
}

# ==============================================================================
# II. Commands API functions
# ------------------------------------------------------------------------------
#' @title Commands Help
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function returns a list of available commands or args.
#' @details Works with or without 'help' command prefix. Note that if you ask about a command that doesn't
#' have any arguments, this function will run the command!
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return List of available commands or args
#' @examples
#' \donttest{
#' commandsHelp()
#' commandsHelp('node')
#' commandsHelp('node get attribute')
#' }
#' @importFrom XML htmlParse
#' @importFrom XML xpathSApply
#' @importFrom httr GET
#' @importFrom utils head
#' @importFrom utils tail
#' @export
commandsHelp<-function(cmd.string='help', base.url = .defaultBaseUrl){
    s=sub('help *','',cmd.string)
    q.url <- .command2getQuery(s,base.url)
    res = GET(q.url)
    if(res$status_code > 299){
        write(sprintf("RCy3::commandsHelp, HTTP Error Code: %d\n url=%s", 
                      res$status_code, q.url), stderr())
        stop()
    } else {
        res.html = htmlParse(rawToChar(res$content), asText=TRUE)
        res.elem = xpathSApply(res.html, "//p", xmlValue)
        res.list = res.elem
        if (length(res.elem)==1){
            res.list = unlist(strsplit(res.elem[1],"\n\\s*"))
        }
        print(head(res.list,1))
        tail(res.list,-1)
    }
}

# ------------------------------------------------------------------------------
#' @title Commands GET
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST query URL, executes a GET
#' request, and parses the result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return A \code{list}, \code{status} or None.
#' @examples
#' \donttest{
#' commandsGET('layout get preferred')
#' commandsGET('network list properties')
#' commandsGET('layout force-directed defaultNodeMass=1')
#' }
#' @importFrom XML htmlParse
#' @importFrom XML xpathSApply
#' @importFrom httr GET
#' @export
commandsGET<-function(cmd.string, base.url = .defaultBaseUrl){
    q.url <- .command2getQuery(cmd.string,base.url)
    res = GET(q.url)
    if(res$status_code > 299){
        write(sprintf("RCy3::commandsGET, HTTP Error Code: %d\n url=%s", 
                      res$status_code, q.url), stderr())
        stop()
    } else {
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
    }
}

# ------------------------------------------------------------------------------
#' @title Commands POST
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST query URL, executes a
#' POST request, and parses the result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return A \code{list}, \code{named list}, \code{status} or None.
#' @examples
#' \donttest{
#' commandsPOST('layout get preferred')
#' commandsPOST('network list properties')
#' commandsPOST('layout force-directed defaultNodeMass=1')
#' }
#' @importFrom RJSONIO fromJSON
#' @importFrom httr POST
#' @importFrom httr content_type_json
#' @export
commandsPOST<-function(cmd.string, base.url = .defaultBaseUrl){
    post.url = .command2postQueryUrl(cmd.string,base.url)
    post.body = .command2postQueryBody(cmd.string)
    res = POST(url=post.url, body=post.body, encode="json", content_type_json())
    if(res$status_code > 299){
        write(sprintf("RCy3::commandsPOST, HTTP Error Code: %d\n url=%s\n body=%s", 
                      res$status_code, URLencode(q.url), q.body), stderr())
        stop()
    } else {
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
    }
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
    cmd.string = sub(" ([[:alnum:]]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    cmd = cmdargs[1]
    if(is.na(cmd)){cmd=""}
    q.cmd = URLencode(paste(base.url, "commands", sub(" ","/",cmd), sep="/"))
    args = cmdargs[2]
    if (is.na(args)){
        q.cmd
    }else{
        args = gsub("\"","",args)
        p = "[[:alnum:]]+="
        m = gregexpr(p,args)
        args1 = unlist(regmatches(args,m))
        args1 = gsub('=','',args1)
        #args1 = unlist(str_extract_all(args,"[[:alnum:]]+(?==)")) # requires stringr lib
        args2 = unlist(strsplit(args," *[[:alnum:]]+="))
        args2 = args2[-1]
        names(args2) <- args1
        q.args = .prepGetQueryArgs(args2)
        paste(q.cmd,q.args,sep="?")
    }
}

# takes a named list and makes a string for GET query urls
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
    cmd.string = sub(" ([[:alnum:]]*=)","XXXXXX\\1",cmd.string)
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
    cmd.string = sub(" ([[:alnum:]]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    args = cmdargs[2]
    
    if (is.na(args))
        args = 'atLeastOneArg=required' #supply a benign "filler" if NULL
    
    args = gsub("\"","",args)
    p = "[[:alnum:]]+="
    m = gregexpr(p,args)
    args1 = unlist(regmatches(args,m))
    args1 = gsub('=','',args1)
    #args1 = unlist(str_extract_all(args,"[[:alnum:]]+(?==)")) # requires stringr lib
    args2 = unlist(strsplit(args," *[[:alnum:]]+="))
    args2 = args2[-1]
    names(args2) <- args1
    return(toJSON(args2))
}