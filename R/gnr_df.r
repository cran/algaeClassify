#' Wrapper function to apply gnr_simple across a data.frame or list of species
#' names. Modified on 11/18/2025 by Vijay Patil (vpatil@usgs.gov) for
#' algaeClassify v2.0.5 (pending approval on CRAN).
#'
#' Provides convienent output with a row per name, to streamline merging with
#' original data.
#'
#' @param df data.frame containing names to check
#' @param name.column integer or character string with column name containing
#' 		  species names
#' @param sourceid integer vector with data source ids.
#' see https://resolver.globalnames.org/sources/
#' @param fuzzy_uninomial boolean. Use fuzzy matching for uninomial names?
#' @param name_type Specify format of matched names. Options are
#' 'canonical_simple' (canonical binomial name), 'canonical_full' (with subspecies
#' or subgenera), or 'with_context' (with author and year appended).
#' @param higher boolean: Return higher taxonomic classifications?
#'
#' @export gnr_df
#'
#' @return new data.frame original names (input_name), information about match
#' type,the best match (match_name), taxonomic status, and other output from
#' gnr_simple(). Will contain a row of NAs if no matches were found for a name.
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' lakegeneva<- genus_species_extract(lakegeneva,'phyto_name')
#' lakegeneva$genus_species <- trimws(paste(lakegeneva$genus,
#'	lakegeneva$species))
#'
#' #checking for matches from all GNRS sources, first 5 rows:
#' lakegeneva.namematches <- gnr_df(lakegeneva,"genus_species")
#' lakegeneva.namematches

gnr_df<-function(df,name.column,sourceid=NULL,
                 fuzzy_uninomial=TRUE, name_type="canonical_full",higher=FALSE)
{
  #base API string
  gnrs.string<-"https://verifier.globalnames.org/api/v1/verifications/"

  name<-df[,name.column]

  gnrs.name<-gsub(' ','+',name)
  if(length(gnrs.name)>1){
    gnrs.name<-paste(gnrs.name,collapse='%7C')
  }

  gnrs.string<-paste0(gnrs.string,gnrs.name,'?')

  #add search parameters
  if(!is.null(sourceid)){
    data_sources<-paste(sourceid,collapse='%7C')
    gnrs.string<-paste0(gnrs.string,"data_sources=",data_sources,"&")
  }

  #df search only returns best match to ensure 1 match per taxa.
  gnrs.string<-paste0(gnrs.string,paste0("all_matches=false"),"&")

  if(fuzzy_uninomial){
    gnrs.string<-paste0(gnrs.string,"fuzzy_uninomial=true","&")
  }else{
    gnrs.string<-paste0(gnrs.string,"fuzzy_uninomial=false","&")
  }

  gnrs.string<-paste0(gnrs.string,"species_group=true")

  con<-curl::curl(gnrs.string)

  results<-try(readLines(con,warn = FALSE),silent=TRUE)

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

  #results dataframe
  if(is.null(result.list$names) |
     (length(unique(result.list$names$matchType))==1 && unique(result.list$names$matchType %in% 'NoMatch'))){
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

  results.output<-result.list$names$bestResult

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
                     data_import_date=results.output$entryDate)

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
