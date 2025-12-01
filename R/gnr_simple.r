#' checks species names against a variety of online databases
#' supports fuzzy partial matching, using the Global Names Resolver
#' (https://resolver.globalnames.org/). Modified on 11/18/2025 by
#' Vijay Patil (vpatil@usgs.gov) for
#' algaeClassify v2.0.5 (pending approval on CRAN).
#'
#' @param name character string binomial scientific name to resolve
#' @param sourceid integer vector with data source ids.
#' see https://resolver.globalnames.org/sources/
#' @param best_match boolean. Should the best match be returned based on score?
#' @param fuzzy_uninomial boolean. Use fuzzy matching for uninomial names?
#' @param name_type Specify format of matched names. Options are
#' 'canonical_simple' (canonical binomial name), 'canonical_full' (with subspecies
#' or subgenera), or 'with_context' (with author and year appended).
#' @param higher boolean: Return higher taxonomic classifications?
#'
#' @export gnr_simple
#'
#' @return new data.frame with name matches, column indicating match type
#' and scores from Global Names Resolver (https://resolver.globalnames.org/).
#' Will contain a row of NAs if no matches found
#'
#' @examples
#' #Visit https://resolver.globalnames.org/data_sources to see all possible
#' #data sources for name checking.
#' name<-"Aphanazomenon flos-aquae"
#' #sourceid=3 for ITIS database
#' gnr_simple(name,sourceid=3) #search for best match from ITIS
#' gnr_simple(name,sourceid=NULL,best_match=FALSE) #search for all matches from any source

gnr_simple<-function(name,sourceid=NULL,best_match=TRUE,fuzzy_uninomial=TRUE,
                     name_type="canonical_full",higher=FALSE)
{
  #base API string
  gnrs.string<-"https://verifier.globalnames.org/api/v1/verifications/"

  gnrs.name<-gsub(' ','+',name)
  gnrs.string<-paste0(gnrs.string,gnrs.name,'?')

 if(!is.null(sourceid)){
    data_sources<-paste(sourceid,collapse='%7C')
    gnrs.string<-paste0(gnrs.string,"data_sources=",data_sources,"&")
  }

  if(best_match){
    gnrs.string<-paste0(gnrs.string,paste0("all_matches=false"),"&")
  }else{
    gnrs.string<-paste0(gnrs.string,paste0("all_matches=true"),"&")
  }

  if(fuzzy_uninomial){
    gnrs.string<-paste0(gnrs.string,"fuzzy_uninomial=true","&")
  }else{
    gnrs.string<-paste0(gnrs.string,"fuzzy_uninomial=false","&")
  }

  gnrs.string<-sub("&$","",gnrs.string)

  con<-curl::curl(gnrs.string)

  results<-try(readLines(con,warn=FALSE),silent=TRUE)

  if(is(results,"try-error"))
  {
    close(con)
    stop("No matches or too many requests. Wait several minutes and retry your search, or select a different data source.")
  }else
  {
    results.pretty<-jsonlite::prettify(results)
    close(con)
  }


  #transform to r list of lists
  result.list<-jsonlite::fromJSON(results.pretty)
  if(result.list$names$matchType=='NoMatch'){
   stop("No Matches")
  }

  #results dataframe
  if(is.null(result.list$names)){
    output<-data.frame(input_name=name,
                       match_name=NA,
                       match_score=NA,
                       match_type=NA,
					   taxonomic_status=NA,
                       data_source=NA,
                       data_import_date=NA
    )
    if(higher){
     output<-cbind(data.frame(Kingdom=NA,Phylum=NA,Class=NA,Order=NA,Family=NA),
                   output)
    }
    return(output)

  }

if(!best_match & 'results' %in% names(result.list$names)){
  results.output<-as.data.frame(result.list$names$results)
}else{
 results.output<- as.data.frame(result.list$names$bestResult)
}

match.name<-if(name_type=='canonical_simple'){
                  results.output$currentCanonicalSimple
               }else if(name_type=='with_context'){
                 results.output$currentName
               }else{
                 results.output$currentCanonicalFull
                 #currentCanonicalFull is default behavior
               }

  output<-data.frame(input_name=name,
                     match_name=match.name,
                    match_score=results.output$sortScore,
                    match_type=result.list$names$matchType,
                    taxonomic_status=results.output$taxonomicStatus,
                    data_source=results.output$dataSourceTitleShort,
                    data_import_date=results.output$entryDate
                    )
  if(higher){
    n<-nrow(results.output)
    higher.df<-data.frame(Kingdom=rep(NA,n),Phylum=rep(NA,n),Class=rep(NA,n),
                          Order=rep(NA,n),Family=rep(NA,n))
    higher.groups.keep<-tolower(names(higher.df))
    if(!is.null(results.output$classificationPath)){
      #not all sources have higher taxonomy. Pad with NAs if missing.
      higher.taxonomy<-results.output$classificationPath
      higher.taxonomy<-sapply(higher.taxonomy,strsplit,split='|',fixed=TRUE)
      higher.classes<-results.output$classificationRanks
      higher.classes<-sapply(higher.classes,strsplit,split='|',fixed=TRUE)
      for(i in 1:n){
        higher.df[i,]<-higher.taxonomy[[i]][match(higher.groups.keep,
                                                  tolower(higher.classes[[i]]))]
      }

    }
    output<-cbind(higher.df,output)
  }

  return(output)

}

