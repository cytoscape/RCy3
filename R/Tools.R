# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/diffusion/v1/52/views/772/diffuse
#diffuse(title=NA, view=NA)

##http://localhost:1234/v1/commands/cybrowser/dialog 
#cyBrowser(title, url, text=NA, id=NA, debug=FALSE) 
#cyBrowserHide(id=NA) s
#cyBrowserShow(id=NA) 
#cyBrowserVersion(obj) 

# ------------------------------------------------------------------------------
#' @title Map Identifiers
#'
#' @description FUNCTION_DESCRIPTION
#' @param column Name of column containing identifiers of type specied by \code{map.from}.
#' @param species Common name for species associated with identifiers, e.g., Human. 
#' See details.
#' @param map.from Type of identifier found in specified \code{column}. See details.
#' @param map.to Type of identifier to populate in new column. See details.
#' @param force.single (optional) Whether to return only first result in cases
#' of one-to-many mappings; otherwise the new column will hold lists of identifiers.
#' Default is TRUE.
#' @param table (optional) Name of table, e.g., node (default), edge or network
#' @param namespace (optional) Namespace of table, e.g., default (default), shared
#' or hidden
#' @param network DESCRIPTION
#' @param base.url DESCRIPTION
#' @details Supported species: Human, Mouse, Rat, Frog, Zebrafish, Fruit fly,
#' Mosquito, Worm, Arabidopsis thaliana, Yeast, E. coli, Tuberculosis.
#' 
#' Supported identifier types (depending on species): Ensembl, Entrez Gene, 
#' Uniprot-TrEMBL, miRBase, UniGene,  HGNC (symbols), MGI, RGD, SGD, ZFIN, 
#' FlyBase, WormBase, TAIR.
#' @return dataframe with map.from and map.to columns. Beware: if map.to is not unique,
#' it will be suffixed with an incrementing number in parentheses, e.g., if 
#' mapIdentifiers is repeated on the same network. However, the original map.to 
#' column will be returned regardless.
#' @examples \donttest{
#' mapped.cols <- mapIdentifiers('name','Yeast','Ensembl','SGD')
#' #          name        SGD
#' #17920  YER145C S000000947
#' #17921  YMR058W S000004662
#' #17922  YJL190C S000003726
#' #...
#' }
#' @export
mapIdentifiers <- function(column, species, map.from, map.to, force.single=TRUE,
                           table='node', namespace='default',
                           network=NULL, base.url=.defaultBaseUrl){
    net.suid <- getNetworkSuid(network)
    tbl <- paste(net.suid,namespace,table, sep=" ")
    
    if(force.single)
        fs <- "true"
    else
        fs <- "false"
    
    all.cols <- getTableColumnNames(table, namespace, network, base.url)
    if(!column %in% all.cols)
        stop(sprintf("ERROR:mapIdentifiers, %s does not exist", column))
    
    commandsPOST(paste0('idmapper map column columnName="',column,
                        '" forceSingle="',fs,
                        '" mapFrom="',map.from,
                        '" mapTo="',map.to,
                        '" species="',species,
                        '" table="',tbl,'"'))
    
    getTableColumns(table=table, columns=c(column,map.to), namespace = namespace, 
                    network = network, base.url = base.url)
}