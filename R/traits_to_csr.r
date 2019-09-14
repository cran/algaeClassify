#' Assign phytoplankton species to CSR functional groups, based on surface to volume ratio and
#' maximum linear dimension ranges proposed by Reynolds et al. 1988;2006
#'
#' @param sav numeric estimate of cell or colony surface area /volume ratio
#' @param msv numeric product of surface area/volume ratio and maximum linear dimension
#' @param msv.source character string with reference source for distinguishing criteria
#' @param traitrange data frame with trait criteria for c,s,r groups. The included table
#'     can be replaced with user-defined criteria if desired. Measurements are:
#'     Surface area/volume ratio (sav), maximum linear dimension (mld) and mld*sav (msv).
#'
#' @export traits_to_csr
#'
#' @return a character string with one of 5 return values: C,CR,S,R, or SR.
#'         CR and SR groups reflect overlap between criteria for the 3 main groups.
#'
#' @examples
#'
#' traits_to_csr(sav=0.2,msv=10,msv.source='Reynolds 2006',traitrange=traitranges)
#'
#'
#' @seealso /url{https://powellcenter.usgs.gov/geisha} for project information


traits_to_csr=function(sav,msv,msv.source='Reynolds 2006',traitrange=traitranges)
{

  csr=NA
  #must be based on measurements in micrometers (^2, ^3)
  sav.vals=unlist(traitrange[traitrange$Measurement=='sav',2:7])

  ##default is to use MSV criteria from Reynolds 2006
  ##but can also use MLD criteria from Reynolds 1988

  ##either option uses SAV criteria from Reynolds et al. 1988
  ##Because Reynolds et al. 2006 does not contain SAV criteria

  if(is.na(sav) | is.na(msv)){csr=NA;return(csr)}


  if(msv.source == 'Reynolds 1988')
  {
    mld.vals=unlist(traitrange[traitrange$Measurement=='mld',2:7])
    msv.vals=sav.vals*mld.vals
  }else{
    msv.vals=unlist(traitrange[traitrange$Measurement=='msv',2:7])
  }

  if(sav>=sav.vals[1] & sav < sav.vals[4] &
     msv >=msv.vals[1] & msv <msv.vals[4]){csr = 'C'}

  if(msv < msv.vals[3] & sav >= sav.vals[5]){csr='C'}

  if(msv < msv.vals[4] & sav >= sav.vals[6]){csr='C'}

  if(sav>=sav.vals[5] & msv >= msv.vals[4]){csr='R'}

  if(sav <sav.vals[5] &
     msv < msv.vals[5]){csr = 'S'}
  if(sav>=sav.vals[3] & sav < sav.vals[6] &
     msv>=msv.vals[3] & msv < msv.vals[6]){csr = 'R'}

  if(sav >=sav.vals[3] & sav < sav.vals[6] &
     msv >=msv.vals[3] & msv < msv.vals[4]){csr = 'CR'}

  if(msv >=msv.vals[5] & sav < sav.vals[3]) {csr='SR'}

  return(csr)

}
