#' checks species names against a variety of online databases
#' supports fuzzy partial matching, using the Global Names Resolver
#' (https://resolver.globalnames.org/)
#'
#' Provides convienent output with a single result, using a variety
#' of criteria for the best match
#'
#' @param name character string binomial scientific name to resolve
#' @param sourceid integer vector with data source ids.
#' see https://resolver.globalnames.org/sources/
#' @param best_match boolean. Should the best match be returned based on score?
#' @param fuzzy_uninomial boolean. Use fuzzy matching for uninomial names?
#' @param canonical boolean. return canonical name?
#' @param with_context boolean. Return context (auther of species name?)
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
#' #sourceid=3 for ITIS database,195 for Algaebase
#' gnr_simple(name,sourceid=3) #search for ITIS matches
#' gnr_simple(name,sourceid=NULL) #search for matches from any source

gnr_simple<-function(name,sourceid=NULL,best_match=TRUE,fuzzy_uninomial=TRUE,
                     canonical=TRUE,with_context=TRUE,higher=FALSE)
{
  #base API string
  gnrs.string<-"https://verifier.globalnames.org/api/v1/verifications/"

  gnrs.name<-gsub(' ','+',name)
  gnrs.string<-paste0(gnrs.string,gnrs.name,'?')
  if(best_match){
    gnrs.string<-paste0(gnrs.string,"best_match_only=",best_match,"%26")
  }
  if(!is.null(sourceid)){
    data_sources<-paste(sourceid,collapse='%7C')
    gnrs.string<-paste0(gnrs.string,"data_source_ids=",data_sources,"%26")
  }
  if(!fuzzy_uninomial){
    gnrs.string<-paste0(gnrs.string,"fuzzy_uninomial=",fuzzy_uninomial,"%26")
  }
  gnrs.string<-paste0(gnrs.string,"species_group=true")
  con<-curl::curl(gnrs.string)

  results<-jsonlite::prettify(try(readLines(con,warn=FALSE),silent=TRUE))
  if(is(results,"try-error"))
  {
    close(con)
    stop("No matches") #throw error
  }

  close(con)

  #transform to r list of lists
  result.list<-jsonlite::fromJSON(results)



  #results dataframe
  if(is.null(result.list$names)){
    output<-data.frame(input_name=name,
                       match_name=NA,
                       match_score=NA,
                       match_type=NA,
                       data_source=NA,
                       data_import_date=NA
    )
    if(higher){
     output<-cbind(data.frame(Kingdom=NA,Phylum=NA,Class=NA,Order=NA,Family=NA),
                   output)
    }
    return(output)

  }


  results.output<-result.list$names$bestResult
  match.name<-if(canonical){results.output$currentCanonicalFull}else{
    results.output$currentName}
  output<-data.frame(input_name=name,
                     match_name=match.name,
                    match_score=results.output$sortScore,
                    match_type=result.list$names$matchType,
                    data_source=results.output$dataSourceTitleShort,
                    data_import_date=results.output$entryDate
                    )
  if(higher){
    n<-nrow(results.output)
    higher.df<-data.frame(Kingdom=rep(NA,n),Phylum=rep(NA,n),Class=rep(NA,n),
                          Order=rep(NA,n),Family=rep(NA,n))
    higher.groups.keep<-tolower(names(higher.df))
    higher.taxonomy<-results.output$classificationPath
    higher.taxonomy<-sapply(higher.taxonomy,strsplit,split='|',fixed=TRUE)
    higher.classes<-results.output$classificationRanks
    higher.classes<-sapply(higher.classes,strsplit,split='|',fixed=TRUE)
    for(i in 1:n){
     higher.df[i,]<-higher.taxonomy[[i]][match(higher.groups.keep,
                                               tolower(higher.classes[[i]]))]
    }
    output<-cbind(higher.df,output)
  }

  return(output)

}

