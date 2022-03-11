#' Database of functional traits for MFG classification, derived from Rimet et al. 2019 
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{phyto_name}{binomial scientific name}
#'  \item{genus}{genus name}
#'  \item{species}{species name}
#'  \item{SAV}{surface area:volume ratio}
#'  \item{MLD}{maximum linear dimension (micrometers)}
#'  \item{MSV}{product of SAV and MLD; unitless}
#'	\item{volume.um3}{cell or colony biovolume}
#'	\item{surface.area.um2}{biological unit (cell or colony) surface area accounting for mucilage}
#'	\item{Colonial}{1/0 indicates colonial growth form}
#'	\item{Number.of.cells.per.colony}{literature-based average colony abundance}
#'	\item{Geometrical.shape.of.the.colony}{Shape descriptions. See Rimet et al. 2019 for abbreviations}
#'  \item{traitCSR}{CSR classification using traits_to_CSR function and criteria from Reynolds 2006}
#' }
#'
#' @docType data
#'
#' @usage data(mfgTraits)
#'
#' @keywords datasets
#' 
"csrTraits"