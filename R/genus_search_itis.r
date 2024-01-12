#' Wrapper function for several functions in ritis::
#' Searches ITIS database for matches to a genus name
#'
#' @param genus Character string. genus name to search for in ITIS
#' @param higher Boolean. If TRUE, add higher taxonomic classifications to output
#'
#' @export genus_search_itis
#'
#' @return input data.frame with matches, current accepted names, synonyms, and higher taxonomy
#'
#'
#' @examples
#' genus='Anabaena'
#' genus_search_itis(genus,higher=FALSE)

genus_search_itis<-function(genus,higher=FALSE)
{
  ##added error trapping to handle ITIS site crashes. 12/18/2023
  itis.test<-RCurl::url.exists("www.itis.gov")
  if(!itis.test){
    message("Warning: Could not connect to ITIS website (www.itis.gov). The site may be
            temporarily down. Please try again later.")
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genus,genus.only=1,synonyms="")
    if(higher==TRUE)
      if(higher){
        higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                              Phylum=NA,Class=NA,Subclass=NA,Order=NA,
                              Family=NA)
        res.df<-cbind(res.df,higher.df)
      }
    return(res.df)
  }

	suppressWarnings(rm(list="res.df"))
	  if(is.na(genus))
	  {
			res.df=data.frame(matched.name=NA,match=0,
							 orig.name.accepted=0,
							 orig.name=genus,genus.only=1,synonyms="")
								   if(higher==TRUE)
		  if(higher){
				higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
									Phylum=NA,Class=NA,Subclass=NA,Order=NA,
									Family=NA)
			res.df<-cbind(res.df,higher.df)
		  }
		  return(res.df)
	  }

	orig.name=genus
	#make sure you just search the genus
	genus<-strsplit(genus,split=' ')[[1]][1]

	accepted=0
	search.q=paste0("rank:Genus AND nameWOInd:",genus)
	s<-tryCatch(suppressWarnings(ritis::itis_search(q=search.q)))
	ss<-s[s[["kingdom"]]!='Animalia',]

	sci.names<-genus_species_extract(s,"nameWOInd")
	genus.names=sci.names[sci.names$species=='' &
                          sci.names$genus==genus,] #exact matches
	genus.names<-genus.names[!is.na(genus.names$genus),]
  if(nrow(genus.names)>1){
 	  genus.names=stats::na.omit(genus.names[1,])

  }
  if(nrow(genus.names)==0){

		res.df=data.frame(matched.name=NA,match=0,
                         orig.name.accepted=0,
                         orig.name=genus,genus.only=1,synonyms="")
			if(higher){
					higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
								Phylum=NA,Class=NA,Subclass=NA,Order=NA,
								Family=NA)
					res.df<-cbind(res.df,higher.df)
			}
	 }else{

      tsn=as.numeric(genus.names$tsn)
      accepted_names<-ritis::accepted_names(tsn=tsn)
      if(length(accepted_names)==0){
        orig.name.accepted=1
        accepted.name=genus
      }else{
        orig.name.accepted=0
        accepted.name=accepted_names$acceptedName
      }

      synonyms<-ritis::synonym_names(tsn)
      synonyms<-ifelse(length(synonyms)==0,"",paste(synonyms$sciName,collapse=','))

      res.df<-data.frame(matched.name=accepted.name,
						 match=1,orig.name.accepted=orig.name.accepted,
                         orig.name=genus,genus.only=1,synonyms=synonyms)
      if(higher==TRUE)
      {
        ranks=c("Kingdom",
                "Subkingdom",
                "Infrakingdom",
				"Phylum",
                "Class",
                "Subclass",
                "Order",
                "Family")
        higher.tax<-ritis::hierarchy_full(tsn)
        higher.tax<-higher.tax[match(ranks, higher.tax$rankname) ,]
        higher.df<-data.frame(t(higher.tax$taxonname))
        names(higher.df)<-ranks
        res.df<-cbind(res.df,higher.df)
      }
  }
      return(res.df)

}
