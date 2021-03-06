% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tables.R
\name{mapTableColumn}
\alias{mapTableColumn}
\title{Map Table Column}
\usage{
mapTableColumn(
  column,
  species,
  map.from,
  map.to,
  force.single = TRUE,
  table = "node",
  namespace = "default",
  network = NULL,
  base.url = .defaultBaseUrl
)
}
\arguments{
\item{column}{Name of column containing identifiers of type specified by 
\code{map.from}.}

\item{species}{Common name for species associated with identifiers, e.g., Human. 
See details.}

\item{map.from}{Type of identifier found in specified \code{column}. See details.}

\item{map.to}{Type of identifier to populate in new column. See details.}

\item{force.single}{(optional) Whether to return only first result in cases
of one-to-many mappings; otherwise the new column will hold lists of identifiers.
Default is TRUE.}

\item{table}{(optional) Name of table, e.g., node (default), edge or network}

\item{namespace}{(optional) Namespace of table, e.g., default (default), shared
or hidden}

\item{network}{(optional) Name or SUID of the network. Default is the "current" 
network active in Cytoscape.}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is http://localhost:1234
and the latest version of the CyREST API supported by this version of RCy3.}
}
\value{
dataframe with map.from and map.to columns. Beware: if map.to is not unique,
it will be suffixed with an incrementing number in parentheses, e.g., if 
mapIdentifiers is repeated on the same network. However, the original map.to 
column will be returned regardless.
}
\description{
Perform identifier mapping using an existing column of supported
identifiers to populate a new column with identifiers mapped to the originals.
}
\details{
Supported species: Human, Mouse, Rat, Frog, Zebrafish, Fruit fly,
Mosquito, Worm, Arabidopsis thaliana, Yeast, E. coli, Tuberculosis.

Supported identifier types (depending on species): Ensembl, Entrez Gene, 
Uniprot-TrEMBL, miRBase, UniGene,  HGNC (symbols), MGI, RGD, SGD, ZFIN, 
FlyBase, WormBase, TAIR.
}
\examples{
\donttest{
mapped.cols <- mapTableColumn('name','Yeast','Ensembl','SGD')
#          name        SGD
#17920  YER145C S000000947
#17921  YMR058W S000004662
#17922  YJL190C S000003726
#...
}
}
