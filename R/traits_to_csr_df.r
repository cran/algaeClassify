#' Add CSR functional group classifications to a dataframe of phytoplankton species, based on surface to volume ratio and
#' maximum linear dimension ranges proposed by Reynolds et al. 1988;2006
#'
#' @param df name of dataframe
#' @param msv character string with name of column that contains maximum linear dimension * surface to volume ratio values
#' @param sav character string with name of column that contains surface to volume ratio values
#' @param msv.source character string with reference source for distinguishing criteria
#' @param traitrange data frame with trait criteria for c,s,r groups. The included table
#'     can be replaced with user-defined criteria if desired. Measurements are:
#'     Surface area/volume ratio (sav), maximum linear dimension (mld) and mld*sav (msv).
#'
#' @export traits_to_csr_df
#'
#' @return a character string with one of 5 return values: C,CR,S,SR, or R
#'
#' @examples
#'
#' csr.df<-data.frame(msv=10,sav=1)
#'
#' csr.df$CSR<-traits_to_csr_df(csr.df,'msv','sav')
#'
#'print(csr.df)

traits_to_csr_df=function(df,sav,msv,msv.source='Reynolds 2006',traitrange=traitranges)
{
  csr=vector(mode='character',length=dim(df)[1])

  for(i in 1:dim(df)[1])
  {
    csr[i]=traits_to_csr(df[[sav]][i],df[[msv]][i],msv.source,traitrange)
  }

  return(csr)
}
