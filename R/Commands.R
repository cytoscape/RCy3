# TODO: pauseDialog(message, base.url) http://localhost:1234/v1/commands/command/pause
# TODO: pauseTimer(message, base.url) http://localhost:1234/v1/commands/command/sleep
# TODO: quitCytoscape(base.url) http://localhost:1234/v1/commands/command/quit 

# ------------------------------------------------------------------------------
#' Command Help
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function returns a list of available commands or args.
#' @details Works with or without 'help' command prefix. Note that if you ask about a command that doesn't
#' have any arguments, this function will run the command!
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return List of available commands or args
#' @export
#' @examples
#' \donttest{
#' commandHelp()
#' commandHelp('node')
#' commandHelp('node get attribute')
#' }
#' @import XML
#' @import httr
#' @importFrom utils head
#' @importFrom utils tail
commandHelp<-function(cmd.string='help', base.url = .defaultBaseUrl){
    s=sub('help *','',cmd.string)
    cmds = GET(command2query(s,base.url))
    cmds.html = htmlParse(rawToChar(cmds$content), asText=TRUE)
    cmds.elem = xpathSApply(cmds.html, "//p", xmlValue)
    cmds.list = cmds.elem
    if (length(cmds.elem)==1){
        cmds.list = unlist(strsplit(cmds.elem[1],"\n\\s*"))
    }
    print(head(cmds.list,1))
    tail(cmds.list,-1)
}

# ------------------------------------------------------------------------------
#' Command Run
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST URL, executes a GET
#' request, and parses the HTML result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return List object
#' @export
#' @examples
#' \donttest{
#' commandRun('layout get preferred')
#' }
#' @import XML
#' @import httr
commandRun<-function(cmd.string, base.url = .defaultBaseUrl){
    
    ##TODO use POST or leave alone for "GET friendly" queries, i.e., guaranteed to be short urls?
    res = GET(command2query(cmd.string,base.url))
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
    res.list
}

# ------------------------------------------------------------------------------
#' Command string to CyREST query URL
#'
#' @description Converts a command string to a CyREST query url.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return cyrest url
#' @export
#' @examples
#' \donttest{
#' command2query('layout get preferred')
#' }
#' @importFrom utils URLencode
command2query<-function(cmd.string, base.url = .defaultBaseUrl){
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
        q.args = paste(args1[1],URLencode(args2[1]),sep="=")
        
        for (i in seq(args1)[-1]){
            arg = paste(args1[i],URLencode(args2[i]),sep="=")
            q.args = paste(q.args,arg,sep="&")
        }
        paste(q.cmd,q.args,sep="?")
    }
}

