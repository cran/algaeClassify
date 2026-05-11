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
#' \donttest{
#' genus='Anabaena'
#' genus_search_itis(genus,higher=FALSE)
#' }

genus_search_itis<-function(genus,higher=FALSE)
{
  suppressWarnings(rm(list="res.df"))
  if(is.na(genus))
  {
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genus,genus.only=1,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
    return(res.df)
  }

  orig.name=genus
  genus<-strsplit(genus,split=' ')[[1]][1]

  raw<-tryCatch(
    itis_json_get("searchByScientificName", list(srchKey=genus), timeout=10),
    error=function(e){
      message("Warning: ITIS search returned an error: ",conditionMessage(e),
              "\nThe ITIS service may be temporarily unavailable.")
      NULL
    }
  )
  s <- if(!is.null(raw) && is.data.frame(raw$scientificNames)) raw$scientificNames else NULL

  if(is.null(s)||nrow(s)==0){
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=orig.name,genus.only=1,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
    return(res.df)
  }

  # Filter: exact genus-only match (no species epithet), exclude Animalia
  genus.names <- s[s$kingdom != 'Animalia' & !is.na(s$unitName1) &
                   s$unitName1 == genus & (is.na(s$unitName2) | s$unitName2 == ''), ]
  genus.names <- genus.names[!is.na(genus.names$tsn), ]

  if(nrow(genus.names)>1){
    genus.names=genus.names[1,]
  }
  if(nrow(genus.names)==0){
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genus,genus.only=1,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
  }else{

    tsn=as.numeric(genus.names$tsn)

    raw_an<-tryCatch(
      itis_json_get("getAcceptedNamesFromTSN", list(tsn=tsn), timeout=10),
      error=function(e){
        message("Warning: could not retrieve accepted name from ITIS: ",conditionMessage(e))
        NULL
      }
    )
    accepted_names <- if(!is.null(raw_an) && is.data.frame(raw_an$acceptedNames))
      raw_an$acceptedNames else list()

    if(length(accepted_names)==0){
      orig.name.accepted=1
      accepted.name=genus
    }else{
      orig.name.accepted=0
      accepted.name=accepted_names$acceptedName
    }

    raw_syn<-tryCatch(
      itis_json_get("getSynonymNamesFromTSN", list(tsn=tsn), timeout=10),
      error=function(e){
        message("Warning: could not retrieve synonyms from ITIS: ",conditionMessage(e))
        NULL
      }
    )
    synonyms_df <- if(!is.null(raw_syn) && is.data.frame(raw_syn$synonyms))
      raw_syn$synonyms else NULL
    synonyms <- ifelse(is.null(synonyms_df)||nrow(synonyms_df)==0, "",
                       paste(synonyms_df$sciName, collapse=','))

    res.df<-data.frame(matched.name=accepted.name,
                       match=1,orig.name.accepted=orig.name.accepted,
                       orig.name=genus,genus.only=1,synonyms=synonyms)
    if(higher==TRUE)
    {
      ranks=c("Kingdom","Subkingdom","Infrakingdom","Phylum",
              "Class","Subclass","Order","Family")
      raw_h<-tryCatch(
        itis_json_get("getFullHierarchyFromTSN", list(tsn=tsn), timeout=10),
        error=function(e){
          message("Warning: could not retrieve higher taxonomy from ITIS: ",conditionMessage(e))
          NULL
        }
      )
      higher.tax <- if(!is.null(raw_h) && is.data.frame(raw_h$hierarchyList))
        raw_h$hierarchyList else NULL

      if(is.null(higher.tax)||nrow(higher.tax)==0){
        higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                              Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      }else{
        # Direct API uses camelCase: rankName / taxonName
        higher.tax<-higher.tax[match(ranks, higher.tax$rankName) ,]
        higher.df<-data.frame(t(higher.tax$taxonName))
        names(higher.df)<-ranks
      }
      res.df<-cbind(res.df,higher.df)
    }
  }
  return(res.df)
}
