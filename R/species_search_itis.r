#' Wrapper function for several functions in ritis::
#' Searches ITIS database for matches to a binomial scientific name
#' outputs matches, current accepted names, synonyms, and higher taxonomy
#'
#' @param genspp Character string. Binomial scientific name with space between genus
#' 		  and species.
#' @param higher Boolean. If TRUE, add higher taxonomic classifications to output
#'
#' @export species_search_itis
#'
#' @return data.frame with submitted name (orig.name), matched name (matched.name),
#' 1/0 flag indicating that original name is currently accepted (orig.name.accepted),
#' 1/0 flag indicating if search was genus_only (for distinguishing genus_search_itis
#' and species_search_itis results), synonyms if any, and higher taxonomy (if
#' higher=TRUE)
#'
#'
#' @examples
#' species="Aphanizomenon flosaquae"
#' species_search_itis(species,higher=FALSE)
species_search_itis<-function(genspp,higher=FALSE)
{
  #depends on ritis
  genspp.orig<-genspp
  genspp<-gsub(" ","\\ ",genspp,fixed=TRUE)
  genspp<-gsub("-","",genspp,fixed=TRUE)

  suppressWarnings(rm(list="res.df"))

  ##added error trapping to handle ITIS site crashes. 12/18/2023
  itis.test<-RCurl::url.exists("http://www.itis.gov")
  if(!itis.test){
    message("Warning: Could not connect to ITIS website (www.itis.gov). The site may be
            temporarily down. Please try again later.")
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genspp.orig,genus.only=0,synonyms="")
    if(higher==TRUE)
      if(higher){
        higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                              Phylum=NA,Class=NA,Subclass=NA,Order=NA,
                              Family=NA)
        res.df<-cbind(res.df,higher.df)

      }
    return(res.df)
  }

  if(is.na(genspp))
  {
  		res.df=data.frame(matched.name=NA,match=0,
                         orig.name.accepted=0,
                         orig.name=genspp,genus.only=0,synonyms="")
						       if(higher==TRUE)
      if(higher){
			higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
								Phylum=NA,Class=NA,Subclass=NA,Order=NA,
								Family=NA)
		  res.df<-cbind(res.df,higher.df)

		  }
	  return(res.df)
  }

  accepted=0
  search.q=paste0("rank:Species AND nameWOInd:",genspp)
  s<-ritis::itis_search(q=search.q)
  s<-s[s[["kingdom"]]!='Animalia',]
  sci.names<-genus_species_extract(s,"nameWOInd")

  sci.names$match.names=trimws(paste(sci.names$genus,sci.names$species))
  sci.names<-sci.names[sci.names$match.names==genspp.orig,] #perfect match
  sci.names<-sci.names[!is.na(sci.names$match.names),]

  if(nrow(sci.names)>1){

	  sci.names=stats::na.omit(sci.names[1,])
  }

  if(nrow(sci.names)==0){

		res.df=data.frame(matched.name=NA,match=0,
                         orig.name.accepted=0,
                         orig.name=genspp.orig,genus.only=0,synonyms="")
      if(higher){
			higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
								Phylum=NA,Class=NA,Subclass=NA,Order=NA,
								Family=NA)
		          res.df<-cbind(res.df,higher.df)
      }
  }else{
		#need better error handling for wrapper.
      tsn=as.numeric(sci.names$tsn)
      accepted_names<-ritis::accepted_names(tsn=tsn)
      if(length(accepted_names)==0){
        orig.name.accepted=1
        accepted.name=genspp
      }else{
        orig.name.accepted=0
        accepted.name=accepted_names$acceptedName
      }

      synonyms<-ritis::synonym_names(tsn)
      synonyms<-ifelse(length(synonyms)==0,"",paste(synonyms$sciName,collapse=','))

      accepted.name<-gsub("\\","",accepted.name,fixed=TRUE)
      res.df<-data.frame(
						 matched.name=accepted.name,match=1,
                         orig.name.accepted=orig.name.accepted,
                         orig.name=genspp.orig,genus.only=0,synonyms=synonyms)
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
