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
#' \donttest{
#' species="Aphanizomenon flosaquae"
#' species_search_itis(species,higher=FALSE)
#' }
species_search_itis<-function(genspp,higher=FALSE)
{
  genspp.orig<-genspp

  suppressWarnings(rm(list="res.df"))

  if(is.na(genspp))
  {
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genspp,genus.only=0,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
    return(res.df)
  }

  raw<-tryCatch(
    itis_json_get("searchByScientificName", list(srchKey=genspp.orig), timeout=10),
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
                      orig.name=genspp.orig,genus.only=0,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
    return(res.df)
  }

  # Parse genus and species from input for exact matching
  parts <- strsplit(trimws(genspp.orig), " ")[[1]]
  genus_part   <- parts[1]
  species_part <- if(length(parts) >= 2) parts[2] else ""

  sci.names <- s[s$kingdom != 'Animalia' & !is.na(s$unitName1) & !is.na(s$unitName2) &
                 s$unitName1 == genus_part & s$unitName2 == species_part, ]
  sci.names <- sci.names[!is.na(sci.names$tsn), ]

  if(nrow(sci.names)>1){
    sci.names=sci.names[1,]
  }

  if(nrow(sci.names)==0){
    res.df=data.frame(matched.name=NA,match=0,
                      orig.name.accepted=0,
                      orig.name=genspp.orig,genus.only=0,synonyms="")
    if(higher){
      higher.df<-data.frame(Kingdom=NA,Subkingdom=NA,Infrakingdom=NA,
                            Phylum=NA,Class=NA,Subclass=NA,Order=NA,Family=NA)
      res.df<-cbind(res.df,higher.df)
    }
  }else{
    tsn=as.numeric(sci.names$tsn)

    raw_an<-tryCatch(
      itis_json_get("getAcceptedNamesFromTSN", list(tsn=tsn), timeout=10),
      error=function(e){
        message("Warning: could not retrieve accepted name from ITIS: ",conditionMessage(e))
        NULL
      }
    )
    accepted_names <- if(!is.null(raw_an) && is.data.frame(raw_an$acceptedNames))
      raw_an$acceptedNames else list()

    ##When the matched record is a synonym/orthographic-variant, accepted_names
    ##returns the accepted name and tsn. Downstream lookups (hierarchy_full,
    ##synonym_names) must use the accepted tsn — the synonym tsn has no parent
    ##in the ITIS hierarchy and tracks no synonyms of its own.
    has_accepted_row<-is.data.frame(accepted_names) && nrow(accepted_names)>0
    if(length(accepted_names)==0 || !has_accepted_row){
      orig.name.accepted=1
      accepted.name=genspp.orig
      accepted.tsn=tsn
    }else{
      orig.name.accepted=0
      accepted.name=accepted_names$acceptedName[1]
      accepted.tsn=as.numeric(accepted_names$acceptedTsn[1])
    }

    raw_syn<-tryCatch(
      itis_json_get("getSynonymNamesFromTSN", list(tsn=accepted.tsn), timeout=10),
      error=function(e){
        message("Warning: could not retrieve synonyms from ITIS: ",conditionMessage(e))
        NULL
      }
    )
    synonyms_df <- if(!is.null(raw_syn) && is.data.frame(raw_syn$synonyms))
      raw_syn$synonyms else NULL
    synonyms <- ifelse(is.null(synonyms_df)||nrow(synonyms_df)==0, "",
                       paste(synonyms_df$sciName, collapse=','))

    res.df<-data.frame(matched.name=accepted.name,match=1,
                       orig.name.accepted=orig.name.accepted,
                       orig.name=genspp.orig,genus.only=0,synonyms=synonyms)
    if(higher==TRUE)
    {
      ranks=c("Kingdom","Subkingdom","Infrakingdom","Phylum",
              "Class","Subclass","Order","Family")
      raw_h<-tryCatch(
        itis_json_get("getFullHierarchyFromTSN", list(tsn=accepted.tsn), timeout=10),
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
