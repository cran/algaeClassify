#' Wrapper function for applying genus_search_itis and species_search_itis
#' to a whole data.frame containing scientific names
#' @param df data.frame containing names to check
#' @param namecol integer or character string with column name containing 
#' 		  species or genus names
#' @param higher Boolean. If TRUE, add higher taxonomic classifications to output
#' @param genus.only boolean If TRUE, search for matches with just the genus name using genus_search_itis
#'
#' @export itis_search_df
#'
#' @return data.frame with submitted names (orig.name), matched names (matched.name),
#' 1/0 flag indicating that original name is currently accepted (orig.name.accepted),
#' 1/0 flag indicating if search was genus_only (for distinguishing genus_search_itis
#' and species_search_itis results), synonyms if any, and higher taxonomy (if 
#' higher=TRUE)
#'
#' @examples
#  
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')[1:5,]
#'
#' #checking for genus-only name matches in ITIS, and extracting higher taxonomy
#' #flagging names with imperfect or no matches
#'
#' lakegeneva.genus.itischeck <- itis_search_df(new.lakegeneva,"genus",genus.only=TRUE)
#' lakegeneva.genus.itischeck

itis_search_df<-function(df,namecol=NA,higher=FALSE,genus.only=FALSE)
{
	suppressWarnings(rm(list="itis.df")) #remove if present
	itis.df<-data.frame(matched.name=NULL,match=NULL,orig.name.accepted=NULL,
	orig.name=NULL,genus.only=NULL, synonyms=NULL)
		if(higher){
			itis.df<-rbind(itis.df,data.frame(Kingdom=NULL, Subkingdom=NULL, Infrakingdom=NULL, Phylum=NULL, Class=NULL,Subclass=NULL,Order=NULL,
			Family=NULL)
		)
	}
	#wrapper for genspp_itis_search and genus_itis_search on whole data.frame
	#
	nrows<-nrow(df)
	for(i in 1:nrows)
	{	
		name=df[[namecol]][i]
		if(!is.na(name) & !is.null(name))
		{
			if(genus.only){
				tmp<-genus_search_itis(name,higher=higher)
			}else{
				tmp<-species_search_itis(name,higher=higher)
			}
			
		}else{
			tmp<-data.frame(matched.name=NA,match=0,orig.name.accepted=0,
			orig.name=NA,genus.only=ifelse(genus.only,1,0),synonyms="")
			if(higher){
				tmp<-cbind(tmp,data.frame(Kingdom=NA, Subkingdom=NA, Infrakingdom=NA, Phylum=NA, Class=NA,Subclass=NA,Order=NA,
				Family=NA)
				)
			}
		}
		itis.df<-rbind(itis.df,tmp)
	}
	return(itis.df)

}