# ------------------------------------------------------------------------------
setClass("CytoscapeConnectionClass", 
         slots = c(uri="character",api="character"), 
         prototype = prototype(uri="http://localhost:1234", api="v1")
)
# Constructor
CytoscapeConnection_DEAD = function(host='localhost', port=1234) {
    res <- .BBSOverride(host, port)
    host = res$host
    port = res$port
    uri = sprintf('http://%s:%s', host, port)
    cc = new('CytoscapeConnectionClass', uri = uri)
    if (!url.exists(uri)){
        write(sprintf('Connection failed.'), stderr())
        write(sprintf('To troubleshoot: 1) Please ensure that you have Cytoscape open'), stderr())
        write(sprintf('2) that the latest version of CyREST is installed.'), stderr())
        write(sprintf('3) that Cytoscape uses Java 8 (not 7 or below).'), stderr())
        write(sprintf('To help troubleshooting, please check:'), stderr())
        write(sprintf('http://www.cytoscape.org/troubleshooting.html'), stderr())
        stop(sprintf('CyREST connection problem. RCy3 can not continue!'))
    }
    cc@api = "v1"
    return(cc)
} # END CytoscapeConnection

#--------------------------------------------------------------------------------
# this code is for the Bioconductor build system. You should never need to set or
# read these environment variables in ordinary use.
.BBSOverride <- function(host, port) {
    ret <- list()
    if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE") != "")) {
        host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE")
        port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE"),"integer")
    }
    if (.Platform$r_arch == "x64") {
        if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64") != "")) {
            host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64")
            port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64"),"integer")
        }
    }
    #cat(paste("Using host", host, "and port", port, "."))
    
    ret["host"] <- host
    ret["port"] <- port
    ret
}

