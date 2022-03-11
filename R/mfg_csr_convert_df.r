#' Returns a CSR classification based on Morphofunctional group (MFG).
#' Correspondence based on Salmaso et al. 2015 and Reynolds et al. 1988
#'
#' @param phyto.df dataframe containing a character field containing MFG classifications
#' @param mfg Character string with MFG name, following Salmaso et al. 2015
#'
#' @export mfg_csr_convert_df
#' 
#' @return A dataframe with an additional field named CSR, containing CSR classifications or NA
#' 
#' @examples
#' 
#' data(lakegeneva)
#' lakegeneva<-genus_species_extract(lakegeneva,'phyto_name')
#' lakegeneva<-species_to_mfg_df(lakegeneva)
#' lakegeneva<-mfg_csr_convert_df(lakegeneva,mfg='MFG')
#' head(lakegeneva)

mfg_csr_convert_df=function(phyto.df,mfg)
{
  csrs<-vector(length=dim(phyto.df)[1])
  for(i in 1:dim(phyto.df)[1])
  {
    csrs[i]=mfg_csr_convert(phyto.df[[mfg]][i])
  }
  phyto.df$CSR=csrs
  return(phyto.df)
}