#' Returns a CSR classification based on Morphofunctional group (MFG).
#' Correspondence based on Salmaso et al. 2015 and Reynolds et al. 1988
#'
#' @param mfg Character string with MFG name, following Salmaso et al. 2015
#'
#' @export mfg_csr_convert
#' 
#' @return A character string with values 'C','S','R','CR','SC','SR', or NA
#' 
#' @examples
#' 
#' mfg_csr_convert("11a-NakeChlor")

#returns a CSR classification for a single MFG
mfg_csr_convert<-function(mfg)
{
  if(mfg %in% algaeClassify::mfg_csr_library$MFG==FALSE){csr=NA}
  else
  {
  	csr=algaeClassify::mfg_csr_library$CSR[algaeClassify::mfg_csr_library$MFG==mfg]
  	return(csr)
  }

}