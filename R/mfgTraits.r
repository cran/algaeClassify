#' Functional Trait Database derived from Rimet et al. 
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{phyto_name}{binomial scientific name}
#'  \item{genus}{genus name}
#'  \item{species}{species name}
#'  \item{Mobility.apparatus}{1/0 indicates presence/absence of flagella or motility}
#'  \item{Size}{character values 'large' or 'small'; based on 35 micrometer max linear dimension}
#'  \item{Colonial}{1/0 indicates typical colonial growth form or not}
#'	\item{Filament}{1/0 indicates filamentous growth form or not}
#'	\item{Centric}{1/0 indicates diatoms with centric growth form}
#'	\item{Gelatinous}{1/0 indicates presence/absence of mucilage}
#'	\item{Aerotopes}{1/0 indicates presence/absence of aerotopes}
#'	\item{Class}{Taxonomic class}
#'  \item{Order}{Taxonomic order}
#'	\item{MFG.fromtraits}{MFG classification using traits_to_mfg function}
#' }
#'
#' @docType data
#'
#' @usage data(mfgTraits)
#'
#' @keywords datasets
#' 
"mfgTraits"