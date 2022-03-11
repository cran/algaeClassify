#' Wrapper function to apply species_phyto_convert() across a data.frame
#'
#' @param phyto.df Name of data.frame. Must have character fields named 'genus' and 'species'
#' @param flag Resolve ambiguous MFG: 1 = return(NA), 2 = manual selection
#' @param mfgDbase specify library of species to MFG associations.
#'
#' @export species_to_mfg_df
#'
#' @return input data.frame with a new character column of MFG classifications
#' and diagnostic information
#'
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' new.lakegeneva <- species_to_mfg_df(new.lakegeneva)
#' head(new.lakegeneva)

species_to_mfg_df <- function(phyto.df,flag=1,mfgDbase=NA)
{
  phyto.len<-dim(phyto.df)[1]
  na.vec<-rep(NA,length=phyto.len)
  # new edit, specify category of columns before end. This appears to be working well for me. Is it faster than using rbind()?
  mfgs<-data.frame(MFG=factor(na.vec, levels = factor(unique(algaeClassify::species_mfg_library$MFG))),ambiguous.mfg=as.numeric(na.vec),genus.classification=as.numeric(na.vec),partial.match=as.numeric(na.vec),flag=as.numeric(na.vec))
  for(i in 1:phyto.len)
  {
    mfgs[i, ] <- species_to_mfg(phyto.df$genus[i],phyto.df$species[i],flag=flag,mfgDbase=mfgDbase)
  }

  # phyto.df<-phyto.df[,names(phyto.df) %in% c('MFG','ambiguous.mfg','genus.classification','partial.match','flag')==FALSE,]
  phyto.df$MFG=c(as.character(paste(mfgs[,1]))) # to make it work I had to include paste()
  # phyto.df$MFG[which(phyto.df$MFG == "NA")] <- NA # weird trick to make it back to the format you had... There is probably a better way to do that
  # phyto.df$MFG<-c(as.character(mfgs[,1]))
  phyto.df$ambiguous.mfg=c(as.character(mfgs[,2]))
  phyto.df$genus.classification=c(as.character(mfgs[,3]))
  phyto.df$partial.match=c(as.character(mfgs[,4]))
  phyto.df$flag=c(as.character(mfgs[,5]))
  return(phyto.df)
}


