#' Conversion of a single genus and species name to a single MFG. Uses species.mfg.library
#'
#' @param genus Character string: genus name
#' @param species Character string: species name
#' @param flag Resolve ambiguous mfg: 1 = return(NA),2= manual selection
#' @param mfgDbase data.frame of species MFG classifications. Defaults to the supplied species.mfg.library data object
#'
#' @export species_to_mfg
#'
#' @return a data frame with MFG classification and diagnostic information.
#' ambiguous.mfg=1 if multiple possible mfg matches
#' genus.classification=1 if no exact match was found with genus + species name
#' partial.match=1 if mfg was based on fuzzy matching of taxonomic name.
#'
#' @examples
#' species_to_mfg('Scenedesmus','bijuga')
#' #returns "11a-NakeChlor"

species_to_mfg<-function(genus,species="",flag=1,mfgDbase=NA)#set flag to two if you want to
													 #manually resolve ambiguous mfg class.
  #default behavior is to set ambiguous classes to NA (flag=1)
{
  if(!is.data.frame(mfgDbase))
  {
	mfgDbase<-algaeClassify::species_mfg_library
  }

  mfgDbase<-mfgDbase[!duplicated(mfgDbase),]

  #create vector for indicator if name is ambiguous or not.
  ambiguous.mfg=0
  #vector indicating if mfg classification was based on genus-level match.
  genus.classification=0

  #vector indicating whether the bestmatch function was used or not.
  partial.match=0

  genus=gsub('Unknown ','',genus)
  if(species %in% mfgDbase$species==F){species=''}#replacing spp., sp. etc. with ''

  #hardcoding a match
  if(genus=='Hyloraphidium'){genus='Hyaloraphidium'}

  if(species==''){genus.classification=1}
  #check for genus and species match first.
  mfg=mfgDbase$MFG[mfgDbase$genus==genus &
							  mfgDbase$species==species]
  #go to genus match
 if(length(unique(mfg)==1))
 {
   mfg=unique(mfg)
 }else{
   species=''
   mfg=mfgDbase$MFG[mfgDbase$genus==genus & mfgDbase$species==species]
   genus.classification=1
 }



  #if there is no genus only match, see if there is another species with the same genus
  if(length(unique(mfg))==0)
  {
    mfg=mfgDbase$MFG[mfgDbase$genus==genus]
    genus.classification=1
  }

  if(length(unique(mfg))==0)
  {
    #try for fuzzy genus matching
    genus.bestmatch<-bestmatch(enteredName=genus,possibleNames=unique(mfgDbase$genus))
    if(genus.bestmatch != 'multiplePartialMatches' & !is.na(genus.bestmatch))
    {
      mfg=mfgDbase$MFG[mfgDbase$genus==genus.bestmatch]
      genus.classification=1
      partial.match=1
    }
  }

  #now, mfg length should be 0, 1, or 2
  if(length(unique(mfg[!is.na(mfg)]))==2)#flag 2 means you can interactively
							#choose among two possible mfgs for a particular genus or species
  {
    ambiguous.mfg=1
    if(flag==1)
    {
      mfg=NA
    }else if (flag==2)
    {
      mfg=unique(mfg)
      cat(paste('\n two possible mfgs for the species: ',genus,species))
      cat()
      cat(paste('\n1:',mfg[1]))
      cat(paste('\n2:',mfg[2]))
      cat(paste('\n3:', NA))
      choice=as.numeric(readline(prompt='\nenter your choice: (1,2,3): \n'))
      mfg=mfg[choice]
    }
  }

  mfg<-unique(mfg[!is.na(mfg)])
  if(length(mfg)==0 )
  {
    mfg=NA
  }else
  {
    mfg=mfg[1]
  }

  mfg.df<-data.frame(MFG=mfg,
                     ambiguous.mfg=ambiguous.mfg,
                     genus.classification=genus.classification,
                     partial.match=partial.match,
                     flag=flag)
  return(mfg.df)
}
