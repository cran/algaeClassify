#' Retrieve taxonomic information from the algaebase online database (www.algaebase.org) based on a user-specified genus and species name . This function requires a valid API key for algaebase.
#'
#' @param genus genus name as character string
#' @param species species name as character string
#' @param apikey valid key for algaebase API as character string
#' @param handle curl handle with API key. Will be created if not present.
#' @param higher boolean should higher taxonomy be included in output?
#' @param print.full.json boolean returns raw json output if TRUE. Default is FALSE (return R data frame)
#' @param newest.only boolean should results be limited to the most recent matching entry in algaebase?
#' @param long boolean return long output including full species name and authorship, and entry date from algaebase.
#' @param exact.matches.only boolean should results be limited to exact matches?
#' @param api_file path to text file containing a valid API key
#'
#' @export algaebase_species_search
#'
#' @importFrom methods is
#'
#' @return data frame that may include: accepted.name (currently accepted synonym if different from input name), input.name (name supplied by user), input.match (1 if exact match, else 0), currently.accepted (1=TRUE/0=FALSE), genus.only (1=genus search/0=genus+species search),higher taxonomy (kingdom,phylum,class,order,family), genus, species (always NA for genus search), infraspecies name (always NA for genus search), long.name (includes author and date if given), taxonomic.status (currently accepted, synonym, or unverified), taxon.rank (taxonomic rank of accepted name (genus, species, infraspecies), mod.date (date when entry was last modified in algaebase).
#'
#' @examples
#'
#' \dontrun{algaebase_species_search("Anabaena flos-aquae")} #not run
#'
#'
algaebase_species_search<-function(genus,species,apikey=NULL,handle=NULL,
                                 higher=TRUE,print.full.json=FALSE,
                                 newest.only=TRUE,long=FALSE,exact.matches.only=TRUE,
								 api_file=NULL){

  #first, throw error if there is no genus name
  #this prevents the code from trying to search every possible name in the
  #database.
  if(genus==''|is.na(genus)|is.null(genus))
  {
    stop("No genus name supplied")
  }

  #parse infraspecific names from specificEpithet, if present
  #and create appropriate search query string.
  #genus_species_extract strips out infraspecific category names
  #so we will just look for the infraspecific name within the full scientific name.
  if(length(grep(" ",species)>0)){
    species.split=strsplit(species,split=' ')[[1]]
    sp=species.split[1]

    #remove weird descriptors from species
    sp<-gsub(r"{\s*\([^*].*}","",sp)
    sp<-gsub(">","",sp)
    sp<-gsub("<","",sp)

    infrasp=species.split[2]

    species.search.string<-paste0("https://api.algaebase.org/v1.3/species?genus=",
                                  genus,"&dwc:specificEpithet=",sp,
                                  "&dwc:scientificName=",infrasp)

    }else{
     species.search.string<-paste0("https://api.algaebase.org/v1.3/species?genus=",genus,
                                  "&dwc:specificEpithet=",species)
}



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

  #submit query
  con <- curl::curl(species.search.string, handle = handle)

  #parse query results
  results<-try(readLines(con),silent=TRUE)
  if(is(results,"try-error"))
  {
    close(con)
    stop("No matches") #throw error for now.
    #will need to modify in wrapper so that it fills with NA instead.
  }

  results<-jsonlite::prettify(results)
  close(con) #need to close the connection asap.

  if(print.full.json){ #can just return the raw output if that is what user wants
    print(results)
    return(results)

  }
  #transform to r list of lists
  result.list<-jsonlite::fromJSON(results)

  #objects

  #so, if no match in the first page of output, you have to keep going.
  #need to deal with the issue of multiple pages more explicitly at some point
  #but it shouldn't be a huge problem most of the time.
  #for now you could return a flag or warning if there are multiple pages of results.

  pagination<-result.list[[1]]
  results.output<-result.list[[2]]


  # num.results<-pagination$"_total_number_of_results"
  # num.pages<-pagination$"_total_number_of_pages"

  #dealing with infraspecific name
  output.infraspname<-ifelse(results.output$"dwc:taxonRank"=="forma",
                                    results.output$infraspecificEpithet_forma,
                                    ifelse(results.output$"dwc:taxonRank"=="variety",
                                           results.output$infraspecificEpithet_variety,
                                           ifelse(results.output$"dwc:taxonRank"=="subspecies",
                                                  results.output$infraspecificEpithet_subspecies,"")))
  output.clean.names<-paste(results.output$"dwc:genus",results.output$"dwc:specificEpithet",output.infraspname)
  output.clean.names<-trimws(output.clean.names)

input.clean.name<-paste(genus,species)

output.match.indices<-output.clean.names==input.clean.name

#only retain exact matches if asked.
if(exact.matches.only){
  if(sum(output.match.indices)==0){stop("No exact matches found")
    }else{
            results.output<-results.output[output.match.indices,];
                        output.match.indices=TRUE
    }
}

long.name<-algaebase_output_parse(results.output,"dwc:scientificName")
taxonomic.status<-algaebase_output_parse(results.output,"dwc:taxonomicStatus")
taxonomic.genus<-algaebase_output_parse(results.output,"dwc:genus")
taxonomic.species<-algaebase_output_parse(results.output,"dwc:specificEpithet")
taxonRank<-algaebase_output_parse(results.output,"dwc:taxonRank")
authorship<-algaebase_output_parse(results.output,"dwc:scientificNameAuthorship")
mod.date<-algaebase_output_parse(results.output,"dcterms:modified")
mod.date<-lubridate::ymd(mod.date) #set as date type so you can sort output based on latest modification
currently.accepted=ifelse(taxonomic.status=='currently accepted taxonomically',1,0)
#need to clean up accepted name section.
accepted.name<-ifelse(currently.accepted==1,
                      algaebase_output_parse(results.output,"dwc:scientificName"),
                      algaebase_output_parse(results.output,"dwc:acceptedNameUsage"))

for(i in 1:length(accepted.name)){
  accepted.name[i]<-gsub(authorship[i],"",accepted.name[i],fixed = TRUE)
  accepted.name[i]<-gsub(r"{\s*\([^*].*}","",accepted.name[i]) #remove text after parens
  accepted.name[i]<-trimws(accepted.name[i])
}

input.name=input.clean.name
input.match=ifelse(output.match.indices,1,0)

output<-data.frame(genus=taxonomic.genus,species=taxonomic.species,infrasp=NA,taxonomic.status,currently.accepted,genus.only=0,accepted.name,input.name=input.clean.name,
                   input.match,taxon.rank=taxonRank,mod.date,long.name,authorship)

for(i in 1:nrow(output)){
  output$infrasp[i]<-ifelse(output$taxon.rank[i]=='variety',results.output$infraspecificEpithet_variety[i],
                         ifelse(output$taxon.rank[i]=='forma',results.output$infraspecificEpithet_forma[i],
                                ifelse(output$taxon.rank[i]=='subspecies',results.output$infraspecificEpithet_subspecies[i],NA)))
}



  if(higher){ #merge higher taxonomy and reorder names of output variable
    higher.taxonomy<-algaebase_genus_search(genus,
                                            return.higher.only = TRUE,
                                            handle=handle,exact.matches.only = TRUE,newest.only = TRUE);

    output<-merge(higher.taxonomy,output,all.y=TRUE,by='genus',sort=FALSE);
    output<-subset(output,select= c('accepted.name','input.name','input.match','currently.accepted','genus.only','kingdom','phylum','class','order','family','genus','species','infrasp',
                                    'long.name','taxonomic.status','taxon.rank','mod.date','authorship'))}else{
    output<-subset(output,select=c('accepted.name','input.name','input.match','currently.accepted','genus.only','genus','species','infrasp',
                                                                     'long.name','taxonomic.status','taxon.rank','mod.date','authorship') )

                                    }
  if(newest.only){
    output<-output[output$mod.date==max(output$mod.date),] #only retain the most recent edit
  }else{
    output<-output[order(output$mod.date,decreasing=TRUE),]
  }


  if(!long){output<-output[,names(output) %in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]}



  return(output)

}
