#' Split a dataframe column with binomial name into genus and species columns.
#'
#' @param phyto.df Name of data.frame object
#' @param phyto.name Character string: field in phyto.df containing species name.
#'
#' @export genus_species_extract
#'
#' @return A data.frame with new character fields 'genus' and 'species'
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' head(lakegeneva) #need to split the phyto_name column
#' new.lakegeneva=genus_species_extract(lakegeneva,'phyto_name')
#'
#' head(new.lakegeneva)

genus_species_extract<-function(phyto.df,phyto.name)
#phyto.name is character string indicating column containing phytoplankton binomial names
{
  spp.list<-as.character(phyto.df[[phyto.name]])

  orig.spp.list=spp.list

  spp.list=iconv(spp.list, to='UTF-8') #switched from ASCII- will retain umlauts, accents, etc.
  spp.list=gsub('Cfr. ','',spp.list,ignore.case=T)
  spp.list=gsub('cf ','',spp.list,ignore.case=T)
  spp.list=gsub('cf.','',spp.list,ignore.case=T)

  ##removing descriptors- may contain useful information, but are not a proper name
  spp.list=gsub('colony','',spp.list,ignore.case=T)
    spp.list=gsub('colonies','',spp.list,ignore.case=T)
	  spp.list=gsub('cells','',spp.list,ignore.case=T)
	    spp.list[spp.list=='cell']=''

  ###cleaning up genus-only records
  genus.only.flag=rep(0,length(spp.list)) #flag for species names with spp. or sp. in them
  genus.only.flag[grep(' sp | sp. | spp | spp. | sp.$| spp.$| sp$| sp1| spp$',
					   spp.list,ignore.case=T)]=1
					   #doesn't account for sp or spp at end of string

  ###flagging and cleaning up records with subspecies or vars
  ###good god these are all the different abbreviations for variety subspecies that I found!
  ###have to account for abbreviations with and without capitalization and upper/lower case
  var.flag=rep(0,length(spp.list)) #vector to indicate if a species name includes a var/subsp/etc.
  var.flag[grep(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph |
				  gr | mor | aff. | aff | f | f. | tab. ',spp.list,ignore.case=T)]=1

  spp.list=gsub(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph |
				  gr | mor | aff. | aff | f | f. | tab. ',' ',spp.list,ignore.case=T)

  ###trimming leftover trailing and leading whitespace
  spp.list=trimws(spp.list,'both')

  genus=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][1])
  species=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][2])
  species[is.na(species)]=''
  species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)
  species[grep('[0-9]',species)]='' ##cut out species names with numbers in them.

  #for the search url, species and var are pasted together with a plus sign.
  #will have to be stripped out if you are returning the search species name.

  var=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][3])
  var<-trimws(var,'both')

  var.flag.test=!(grepl("^[[:upper:]]",var) | substr(var,1,1)=="(" | grepl("^[0-9]",var))
  var.flag[var.flag.test==1]=1

  #not using line below
  #var[grep('comb. nov.',orig.spp.list)]='comb.+nov.'

  #pasting species and subspecies/var into a single string.
  species[var.flag==1 & !is.na(var)]=paste(species[var.flag==1 & !is.na(var)],
		  var[var.flag==1 & !is.na(var)],sep=' ')

  genus[is.na(genus)]=''
  species[is.na(species)]=''
  species[genus.only.flag==1]=''

  genus=trimws(genus,'both')
  species=trimws(species,'both')

  phyto.df$genus=c(genus)
  phyto.df$species=c(species)

  return(phyto.df)
}
