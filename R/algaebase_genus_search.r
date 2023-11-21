#' Search algaebase for information about a genus of phytoplankton
#'
#' @param genus genus name as character string
#' @param apikey valid key for algaebase API as character string
#' @param handle curl handle with API key. Will be created if not present.
#' @param higher boolean should higher taxonomy be included in output?
#' @param print.full.json boolean returns raw json output if TRUE. Default is FALSE (return R data frame)
#' @param newest.only boolean should results be limited to the most recent matching entry in algaebase?
#' @param long boolean return long output including full species name and authorship, and entry date from algaebase.
#' @param exact.matches.only boolean should results be limited to exact matches?
#' @param return.higher.only boolean should output only included higher taxonomy?
#' @param api_file path to text file containing a valid API key
#'
#' @export algaebase_genus_search
#'
#' @importFrom methods is
#'
#' @return data frame that may include: accepted.name (currently accepted synonym if different from input name), input.name (name supplied by user), input.match (1 if exact match, else 0), currently.accepted (1=TRUE/0=FALSE), genus.only (1=genus search/0=genus+species search),higher taxonomy (kingdom,phylum,class,order,family), genus, species (always NA for genus search), infraspecies name (always NA for genus search), long.name (includes author and date if given), taxonomic.status (currently accepted, synonym, or unverified), taxon.rank (taxonomic rank of accepted name (genus, species, infraspecies), mod.date (date when entry was last modified in algaebase).
#'
#' @examples
#' \dontrun{algaebase_genus_search("Anabaena")} #not run.
#'

algaebase_genus_search<-function(genus=NULL,apikey=NULL,handle=NULL,
                                 higher=TRUE,print.full.json=FALSE,
                                 newest.only=TRUE,long=FALSE,
                                 exact.matches.only=TRUE,
                                 return.higher.only=FALSE,
								 api_file=NULL){

  if(genus==''|is.na(genus)|is.null(genus))
  {
    stop("No genus name supplied")
  }

  genus.search.string<-paste0('https://api.algaebase.org/v1.3/genus?genus=',genus)

  #check for api key in Sys.env
  #read from file if not specified or available as env variable.
  if(is.null(api_file) & is.null(apikey)){
	apikey<-get_apikey()
  }else if(!is.null(api_file)){
	apikey<-get_apikey_fromfile(api_file)
  }

  #set curl handle if not supplied as an argument
  if(is.null(handle)){
    handle<-set_algaebase_apikey_header(apikey)
  }

  con <- curl::curl(genus.search.string, handle = handle)
  results<-jsonlite::prettify(try(readLines(con),silent=TRUE))
  if(is(results,"try-error"))
  {
    close(con)
    stop("No matches") #throw error for now.
    #will need to modify in wrapper so that it fills with NA instead.
  }

  close(con)

  if(print.full.json){ #can just return the raw output if that is what user wants
    print(results)
    return(results)

  }

  #transform to r list of lists
  result.list<-jsonlite::fromJSON(results)


  #objects
  pagination<-result.list[[1]]
  results.output<-result.list[[2]]
  # num.results<-pagination$`_total_number_of_results`
  # print(num.results) #remove this later- how many results are there?
  # names(results.output) #field names for each hit

  #all of this could be condensed in later versions of the function
  #but if it runs quickly then who cares.
  #can still add a delay between api requests if necessary?

  #adding extra option to be extract higher taxonomy for species searches
  if(return.higher.only){
    higher=TRUE; exact.matches.only=TRUE; newest.only=TRUE

  }

  if(higher==TRUE){
    kingdom<-algaebase_output_parse(results.output,"dwc:kingdom")
    phylum<-algaebase_output_parse(results.output,"dwc:phylum")
    taxonomic.class<-algaebase_output_parse(results.output,"dwc:class")
    taxonomic.order<-algaebase_output_parse(results.output,"dwc:order")
    taxonomic.family<-algaebase_output_parse(results.output,"dwc:family")
    taxonomic.genus<-algaebase_output_parse(results.output,"dwc:genus")

    higher.taxonomy<-data.frame(kingdom=kingdom,phylum,class=taxonomic.class,
                                  order=taxonomic.order,
                                  family=taxonomic.family,
                                genus=taxonomic.genus)

    mod.date<-algaebase_output_parse(results.output,"dcterms:modified")
    mod.date<-lubridate::ymd(mod.date) #set as date type so you can sort output based on latest modification

    if(return.higher.only==TRUE){
      if(exact.matches.only){
        if(sum(taxonomic.genus==genus)==0){stop("No exact matches found")
        }else{higher.taxonomy<-higher.taxonomy[taxonomic.genus==genus,]}
      }

      if(newest.only){
        higher.taxonomy<-higher.taxonomy[mod.date==max(mod.date),] #only retain the most recent edit
      }else{
        higher.taxonomy<-higher.taxonomy[order(mod.date,decreasing=TRUE),]
      }

    return(higher.taxonomy)
    }
  }

  long.name<-algaebase_output_parse(results.output,"dwc:scientificName")
  taxonomic.status<-algaebase_output_parse(results.output,"dwc:taxonomicStatus")
  taxonRank<-algaebase_output_parse(results.output,"dwc:taxonRank")
  authorship<-algaebase_output_parse(results.output,"dwc:scientificNameAuthorship")
  mod.date<-algaebase_output_parse(results.output,"dcterms:modified")
  mod.date<-lubridate::ymd(mod.date) #set as date type so you can sort output based on latest modification
  accepted.name<-algaebase_output_parse(results.output,"dwc:acceptedNameUsage")
  input.name=genus
  input.match=ifelse(genus==taxonomic.genus,1,0)
  currently.accepted=ifelse(taxonomic.status=='currently accepted taxonomically',1,0)
  accepted.name<-ifelse(currently.accepted==1,taxonomic.genus,accepted.name)

  output<-data.frame(genus=taxonomic.genus,species=NA,infrasp=NA,taxonomic.status,currently.accepted,accepted.name,genus.only=1,input.name=genus,
                     input.match,taxon.rank=taxonRank,mod.date,long.name,authorship)
  if(higher){output<-cbind(higher.taxonomy,output);
              output<-subset(output,select= c('accepted.name','input.name','input.match','currently.accepted','genus.only','kingdom','phylum','class','order','family','genus','species','infrasp',
                                                         'long.name','taxonomic.status','taxon.rank','mod.date','authorship'))}else{
              output<-subset(output,select=c('accepted.name','input.name','input.match','currently.accepted','genus.only','genus','species','infrasp',
                                                                        'long.name','taxonomic.status','taxon.rank','mod.date','authorship') )
                                                         }


  #only retain exact matches if asked.
  if(exact.matches.only){
    if(sum(output$input.match)==0){stop("No exact matches found")
    }else{output<-output[output$input.match==1,]}
  }

  if(newest.only){
    output<-output[output$mod.date==max(output$mod.date),] #only retain the most recent edit
  }else{
    output<-output[order(output$mod.date,decreasing=TRUE),]
  }

  if(!long){output<-output[,names(output) %in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]}






    return(output)
}
