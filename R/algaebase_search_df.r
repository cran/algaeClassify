#' Search algaebase for information about a list of phytoplankton names
#'
#' @param df data frame containing columns with genus and species names
#' @param apikey valid key for algaebase API as character string
#' @param genus.only boolean: should searches be based solely on the genus name?
#' @param genus.name name of data.frame column that contains genus names
#' @param species.name name of data.frame column that contains species names
#' @param handle curl handle with API key. Will be created if not present.
#' @param higher boolean should higher taxonomy be included in output?
#' @param print.full.json boolean returns raw json output if TRUE. Default is FALSE (return R data frame)
#' @param long boolean return long output including full species name and authorship, and entry date from algaebase.
#' @param exact.matches.only boolean should results be limited to exact matches?
#' @param api_file path to text file containing a valid API key
#' @param sleep.time delay between algaebase queries (in seconds). Should be at least 1 second if querying more than 10 names at once.
#'
#' @export algaebase_search_df
#'
#' @importFrom methods is
#'
#' @return data frame that may include: accepted.name (currently accepted synonym if different from input name), input.name (name supplied by user), input.match (1 if exact match, else 0), currently.accepted (1=TRUE/0=FALSE), genus.only (1=genus search/0=genus+species search),higher taxonomy (kingdom,phylum,class,order,family), genus, species (always NA for genus search), infraspecies name (always NA for genus search), long.name (includes author and date if given), taxonomic.status (currently accepted, synonym, or unverified), taxon.rank (taxonomic rank of accepted name (genus, species, infraspecies), mod.date (date when entry was last modified in algaebase).
#'
#' @examples
#' \dontrun{
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' lakegeneva.algaebase<-algaebase_search_df(new.lakegeneva[1:10,],higher=TRUE,long=TRUE)
#' head(lakegeneva.algaebase)}
#'
algaebase_search_df<-function(df,apikey=NULL,handle=NULL,genus.only=FALSE,
                              genus.name='genus',species.name='species',
                              higher=TRUE,print.full.json=FALSE,
                              long=FALSE,exact.matches.only=TRUE,
                              api_file=NULL,sleep.time=1)
{

  algaebase_df<-data.frame()

  nrows=nrow(df)
  err.row.func<-function(df,index=0,genus.only=FALSE,
                         genus.name='genus',
                         species.name='species',higher=TRUE,long=TRUE){

      if(genus.only){input.name<-df[[genus.name]][index]}else{
      input.name<-trimws(paste(df[[genus.name]][index],df[[species.name]][index]))
    }

    err.df.row<-data.frame(kingdom=NA,phylum=NA,class=NA,order=NA,family=NA,
                           genus=NA,species=NA,infrasp=NA,taxonomic.status=NA,
                           currently.accepted=NA,accepted.name=NA,genus.only=NA,
                           input.name=input.name,input.match=0,taxon.rank=NA,
                           mod.date=NA,long.name=NA,authorship=NA)


    if(higher==FALSE){
      err.df.row<-err.df.row[,names(err.df.row)%in% c('kingdom','phylum','class','order','family')==FALSE]
    }

    if(long==FALSE){
      err.df.row<-err.df.row[,names(err.df.row)%in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]
    }

    return(err.df.row)
  }

    for(i in 1:nrows){


      Sys.sleep(sleep.time)
      genus=df[[genus.name]][i]
      species=df[[species.name]][i]

      #if no species match, try genus
      #if no genus match, fill row with NAs
      #if no exact matches specified, will return newest hit
      #not smart enough yet to return closest non-exact match.
      #don't get stuck trying to find all genera ever if no genus name supplied.
      if(genus==''|is.na(genus)|is.null(genus)){
                tmp<-err.row.func(df=df,index=i,
                        genus.only=genus.only,
                        genus.name=genus.name,
                        species.name=species.name,
                        higher=higher,long=long)}else if(genus.only|species==''|is.na(species)|is.null(species)){

        tmp<-try(algaebase_genus_search(genus=genus,apikey=apikey,handle=handle,
                                    higher=higher,
                                    print.full.json=FALSE,
                                    newest.only=TRUE,long=long,
                                    exact.matches.only=exact.matches.only,
                                    api_file=api_file,
                                    return.higher.only=FALSE),silent=TRUE)

      }else{
        tmp<-try(algaebase_species_search(genus=genus,species=species,apikey=apikey,handle=handle,
                                    higher=higher,
                                    print.full.json=FALSE,
                                    newest.only=TRUE,long=long,
                                    exact.matches.only=exact.matches.only,
                                    api_file=api_file),silent=TRUE)
        if(is(tmp,'try-error')){
          tmp<-try(algaebase_genus_search(genus=genus,apikey=apikey,handle=handle,
                                          higher=higher,
                                          print.full.json=FALSE,
                                          newest.only=TRUE,long=long,
                                          exact.matches.only=exact.matches.only,
                                          api_file=api_file,
                                          return.higher.only=FALSE),silent=TRUE)
        }
      }

      if(is(tmp,'try-error')){
        tmp<-err.row.func(df=df,index=i,
                                  genus.only=genus.only,
                                  genus.name=genus.name,
                                  species.name=species.name,
                                  higher=higher,long=long)
      }

      print(paste0(round(100*i/nrows),"% complete"))
      algaebase_df<-rbind(algaebase_df,tmp)
    }


    return(algaebase_df)
}

#only difference is return.higher.only
