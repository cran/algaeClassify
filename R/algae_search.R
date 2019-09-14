#' Compare a genus and species name against the algaebase online database
#'
#' @param genus Character string
#' @param species Character string
#' @param long if TRUE, returns higher taxonomy (Kingdom through Family)
#'
#' @export algae_search
#'
#' @import RCurl
#' @import httr
#' @import XML
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @import plyr
#' @importFrom utils write.csv
#' @importFrom stats rnbinom
#' @importFrom xml2 read_html
#'
#' @return A data.frame with the following fields: orig.name (submitted name),
#'     match.name (best match), genus and species (from the match.name), exact.match (1
#'     indicates whether there was a perfect match for orig.name. 0 otherwise),
#'     accepted (1 if the orig.name currently accepted, 0 otherwise),
#'     synonyms (currently accepted synonyms, if any. For genus-only search, returns
#'     genera associated with species formerly classified with the orig.name genus)
#'     Empire,Kingdom,Phylum,Class,Order,Family: character strings with current higher
#'     taxonomy associated with match.name.
#'
#' @examples
#' algae_search(genus='Anabaena',species='flos-aquae',long=FALSE)
#'
#' @seealso \url{http://www.algaebase.org} for phytoplankton taxonomy database,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information.
#'     Algaebase should be cited separately in any publications using this function:
#'     <doi:10.7872/crya.v35.iss2.2014.105>

algae_search=function(genus,species='',long=FALSE)
{
  synonym.swapped<-0

  status<-0
  #This variable is used to flag search results based on whether a single page
  #or a table of results is returned by the website.

  #cleans up abbreviations associated with genus only search
  species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)

  #removing outdated hyphens that mess up regular expression searches
  species<-gsub('flos-aquae','flosaquae',species)

  ##replacing gaps in species subspp name and creating url query
  species<-gsub(' ','+',species)
  if(species=='')
  {
    URL<-paste('www.algaebase.org/search/?species=',
              genus,sep='')
  }else
  {
    URL<-paste('www.algaebase.org/search/?species=',
              genus,'%20',
              species,sep='')
  }

  groups <- c('Empire','Kingdom','Phylum','Class','Order','Family')

  #if no genus is specified, or genus is NA, return a data.frame with NAs
  if(is.na(genus)|genus=='NA'|genus=='NaN'|genus=='na'|genus=='')
  {
    res.df<-data.frame(genus=NA,species=NA,exact.match=0,accepted=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
    if(long)
    {
      res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
    }
    return(res.df)
  }

  #retrieve search result
  url.get<-GET(URL)

  #check for accepted taxonomic status
  #next section determines whether
  # there is no search result returned
    #in this case, the species name will be truncated and search repeated
  #status=1) there is an exact accepted match
  #status=2) there is an exact outdated match with an accepted synonym
  #status=3) there is an unverified exact match
  #status=0) there is a table of multiple potential matches

  parsed<-htmlParse(url.get)
  plain.text <- xpathSApply(parsed, "//p", xmlValue)
  status<-plain.text[grep('Status',plain.text)] ##need to work on this part.
  if(length(grep('click on the name',plain.text))>0)
  {
    status<-0
  } #first, see if multiple results were returned.

  if(length(
     grep('This is a preliminary entry and has not been subject to full verification',
          plain.text))==1)
  {
    status<-3
    accepted<-0
    exact.match<-1
  }else if
  (length(status)==0)
  {
    status<-0
    accepted<-0
    exact.match<-0
    #No search results returned.

    #try again with a truncated species name (without last 4 letters
    sp.name.len<-nchar(species)
    if(sp.name.len>5)
    {
      sp.trunc<-substr(species,1,sp.name.len-5)
    }else(sp.trunc<-substr(species,1,2))

    URL<-paste('www.algaebase.org/search/?species=',
              genus,'%20',
              sp.trunc,sep='')
    url.get<-GET(URL)

    parsed<-htmlParse(url.get)
    plain.text <- xpathSApply(parsed, "//p", xmlValue)
    status<-plain.text[grep('Status',plain.text)] ##need to work on this part.
    if(length(
      grep('This is a preliminary entry and has not been subject to full verification',
           plain.text))==1)
      {
        status<-3
      }else if(length(status)==0)
      {
        status<-0
        accepted<-0
        exact.match<-0

      }else if(
              length(
              grep("This name is of an entity that is currently accepted taxonomically",
                   status))==1)
        #single page result with exact match
      {
        status<-1
        exact.match<-0
        accepted<-1
      }else if(length(
        grep('This name is currently regarded as a synonym of ',status))==1)
        #single page result. entered name is synonym
      {
        match.name<-status
        status<-2
        exact.match<-0
        accepted<-0
      }else
      {
        status<-0
        accepted<-0
        exact.match<-0
        res.species<-gsub('+',' ',species,fixed=T)

      }
  }else if(
    length(
      grep("This name is of an entity that is currently accepted taxonomically",status))
        ==1) #single page result with exact match
  {
    status<-1
    exact.match<-1
    accepted<-1
  }else if(length(grep('This name is currently regarded as a synonym of ',status))==1)
    #single page result. entered name is synonym
  {
    match.name<-status
    status<-2
    exact.match<-1
    accepted<-0
  }else
  {
    status<-0
  }

  if(status==1)
  {
    species<-gsub('+',' ',species,fixed=T)
    orig.name<-paste(genus,species)

    match.name<-plain.text[grep('Publication details',plain.text)]
    match.name<-gsub('Publication details',"",match.name)
    match.name<-algaeClassify::genus_species_extract(data.frame(match.name),"match.name")

    res.genus<-match.name$genus
    res.species<-match.name$species
    match.name<-paste(match.name$genus,match.name$species)

    res.synonyms<-'' #avoid returning homotypic synonyms to avoid confusion
    res.df<-data.frame(genus=res.genus,
                       species=res.species,
                       exact.match=exact.match,
                       accepted=accepted,
                       synonyms=paste(res.synonyms,collapse=','),
                       orig.name=orig.name,
                       match.name=match.name)
    if(long)
    {
      details.parsed<-read_html(url.get)
      classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
      taxa.levels <- html_text(html_nodes(classification.node,"i"))
      taxa<-	html_text(html_nodes(classification.node,"a"))
      df<-data.frame(rbind(taxa))
      names(df)<-taxa.levels
      df<-df[,match(groups,names(df))] #make sure there is a consistent set of names

      res.df<-cbind(res.df,df)
    }
    return(res.df)
  }else if(status==2)###single match, but submitted name is a synonym of accepted name
  {
    species<-gsub('+',' ',species,fixed=T)

    match.name<-gsub('Status of nameThis name is currently regarded as a synonym of ',
                     '',
                     match.name)

    #if exact match with 1 synonym, then it uses synonym as match.name
    match.name<-data.frame(match.name=match.name)
    match.name<-algaeClassify::genus_species_extract(match.name,'match.name')
    res.genus<-match.name$genus
    res.species<-match.name$species

    orig.name<-paste(genus,species)
    match.name<-paste(res.genus,res.species)
    res.synonyms<-match.name

    res.df<-data.frame(genus=res.genus,
                       species=res.species,
                       exact.match=exact.match,
                       accepted=accepted,
                       synonyms=res.synonyms,
                       orig.name=orig.name,
                       match.name=match.name)

    if(long)##grab higher taxonomy for accepted synonym
    {
      synonym.name.df<-data.frame(syn=match.name)
      synonym.name.df<-algaeClassify::genus_species_extract(synonym.name.df,"syn")
      long.df<-algae_search(synonym.name.df$genus,synonym.name.df$species,long=T)[,8:13]
      #retrieve higher taxonomy for synonym match
      res.df<-cbind(res.df,long.df)
      return(res.df)
    }

    return(res.df)
  }else if(status==3)###unverified/unaccepated match
  {
    res.genus<-genus
    res.species<-gsub('+',' ',species,fixed=T)
    orig.name<-paste(genus,species)
    match.name<-paste(genus,species)
    res.synonyms<-plain.text[grep('Synonym',plain.text)]
    res.synonyms<-ifelse(grepl('Homotypic',res.synonyms,fixed=T),
                        gsub('Homotypic Synonym(s)','',res.synonyms,fixed=T),
                        gsub('Synonym(s)','',res.synonyms,fixed=T))
    res.synonyms<-gsub('Heterotypic ','',res.synonyms,fixed=T)
    res.synonyms[grepl('No synonym',res.synonyms,fixed=T)]<-''
    res.df<-data.frame(genus=res.genus,
                       species=res.species,
                       exact.match=exact.match,
                       accepted=accepted,
                       synonyms=paste(res.synonyms,collapse=','),
                       orig.name=orig.name,
                       match.name=match.name)
    if(long)
    {
      details.parsed<-read_html(url.get)
      classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
      taxa.levels <- html_text(html_nodes(classification.node,"i"))
      taxa<-	html_text(html_nodes(classification.node,"a"))
      df<-data.frame(rbind(taxa))
      names(df)<-taxa.levels
      df<-df[,match(groups,names(df))] #make sure there is a consistent set of names

      res.df<-cbind(res.df,df)
    }
    return(res.df)

  }



  tabs<-readHTMLTable(parsed)
  if(length(tabs)==0)
  {
    res.df<-data.frame(genus=NA,
                       species=NA,
                       exact.match=0,
                       accepted=0,
                       synonyms=NA,
                       orig.name=paste(genus,species),
                       match.name=NA)
    if(long)
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
    }
    return(res.df)

  }

  results.tab<-data.frame(tabs[[1]],stringsAsFactors = F)
  results.tab<-algaeClassify::genus_species_extract(results.tab,'Name')
  #cleaning up results names

  results.tab$Name<-paste(results.tab$genus,results.tab$species)

  #first, check for genus only match
  genus.match<-grepl(genus,results.tab[[1]])
  if(sum(genus.match)==0)
  {
    res.df<-data.frame(genus=NA,
                       species=NA,
                       exact.match=0,
                       accepted=0,
                       synonyms=NA,
                       orig.name=paste(genus,species),
                       match.name=NA)
    if(long)
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
    }
    return(res.df)

  }
  results.tab<-results.tab[genus.match,]

  #exit out if results table is size 0
  if(dim(results.tab)[1]==0)
  {
    res.df<-data.frame(genus=NA,
                       species=NA,
                       exact.match=0,
                       accepted=0,
                       synonyms=NA,
                       orig.name=paste(genus,species),
                       match.name=NA)
    if(long)
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
    }
    return(res.df)
  };


  colnames(results.tab)[2]<-'Current name if different'
  results.tab2<-results.tab
  results.tab2[,2]<-as.character(results.tab2[,2])

  ##not excluding unchecked names for now. but flagging them.

  results.tab2$Unchecked<-0
  results.tab2$Unchecked[grep('Unchecked', results.tab2$Name)]<-1
  results.tab2<-algaeClassify::genus_species_extract(results.tab2,'Name')
  #cleaning up results names

  results.tab2$Name<-paste(results.tab2$genus,results.tab2$species)
  results.tab2<-results.tab2[!duplicated(results.tab2),]

  if(species=='') ##only return genus matches if only genus is entered.
  {
    results.tab2$Name<-sapply(results.tab2$Name,function(x){
      return(strsplit(x,split=' ')[[1]][1])
    })
    results.tab2[,2]<-sapply(results.tab2[,2],function(x){
      return(strsplit(x,split=' ')[[1]][1])
    })
    results.tab2[,2][is.na(results.tab2[,2])]<-''
    results.tab2$species<-''
  }else #clean up synonym names
  {

    synonym.tab<-data.frame(synonyms=results.tab2[,2])
    synonym.tab<-algaeClassify::genus_species_extract(synonym.tab,'synonyms')
    results.tab2[,2]<-paste(synonym.tab$genus,synonym.tab$species)
  }
  res.names<-as.character(results.tab2$Name)

  sub.name<-ifelse(species=='',genus,paste(genus,species))
  sub.name<-gsub('+',' ',sub.name,fixed=T)

  match.name<-bestmatch(sub.name,unique(res.names))
  match.rows<-res.names == match.name
  match.tab<-results.tab2[match.rows,]

  match.tab[,2]<-ifelse(match.tab[,2]==match.tab[,1]," ",match.tab[,2])
  match.tab<-match.tab[!duplicated(match.tab),]

  if(is.na(match.name)) #no decent match
  {
    res.df<-data.frame(genus=NA,
                       species=NA,
                       exact.match=0,
                       accepted=0,
                       synonyms=NA,
                       orig.name=paste(genus,species),
                       match.name=NA)
    if(long)
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
    }
    return(res.df)

  }
  results.tab<-results.tab[genus.match,]

  res.synonyms<-paste(unique(match.tab[,2]),collapse=',')
  res.synonyms<-trimws(res.synonyms,'both')
  res.synonyms<-gsub(", ,",",,",res.synonyms)
  res.synonyms<-gsub(",,",",",res.synonyms,fixed=T)
  res.synonyms<-gsub("^[,]","",res.synonyms) #remove preceeding commas
  res.synonyms<-gsub("[,]$","",res.synonyms) #remove trailing commas

  #not automatically deleting synonyms for genus only matches
  #also not auto swapping with single synonym for genus
  exact.match<-ifelse(match.name==sub.name,1,0)
  if(res.synonyms !='' &
     exact.match==1 &
     length(grep(',',res.synonyms))==0 &
     species!='')
  {
    synonym.swapped<-1
    match.name<-res.synonyms
  } #swap synonym in for match.name

  if(res.synonyms=='' &
     sum(match.tab$Unchecked)==0 &
     match.name != 'multiplePartialMatch')
  {
    accepted<-1
  }else
  {
    accepted<-0
  }

  #not excluding unchecked names, just flagging them for now.

  if(match.name=='multiplePartialMatch')
  {
    res.gen.spp<-data.frame(genus='',species='')
    #check if there are any verified names.
    res.df<-data.frame(genus=NA,
                       species=NA,
                       exact.match=0,
                       accepted=0,
                       synonyms=NA,
                       orig.name=sub.name,
                       match.name=match.name)
  }else
  {
    res.gen.spp<-algaeClassify::genus_species_extract(data.frame(res=match.name),'res')


    res.genus<-res.gen.spp$genus
    res.species<-res.gen.spp$species

    #check if there are any verified names.
    res.df<-data.frame(genus=res.genus,
                       species=res.species,
                       exact.match=exact.match,
                       accepted=accepted,
                       synonyms=res.synonyms,
                       orig.name=sub.name,
                       match.name=match.name)
  }
  if(long)
  {
    if(match.name=='multiplePartialMatch')
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
      return(res.df)
    }
    if(synonym.swapped==1)
    {
      #swapping name with synonym in results tab for higher taxonomy lookup
      results.tab2$Name<-res.synonyms

      synonym.name.df<-data.frame(syn=match.name)
      synonym.name.df<-algaeClassify::genus_species_extract(synonym.name.df,"syn")
      long.df<-algae_search(synonym.name.df$genus,synonym.name.df$species,long=T)[,8:13]
      #retrieve higher taxonomy for synonym match
      res.df<-cbind(res.df,long.df)
      return(res.df)

    }
    if(status==1 | status==3) ##single match
    {
      details.parsed<-read_html(url.get)
    }else
    {
      links.parsed<-xpathSApply(parsed,"//a/@href")

      links.parsed<-links.parsed[grep('results',links.parsed)]
      links.parsed<-links.parsed[seq(1,length(links.parsed),by=2)]
      links.parsed<-links.parsed[genus.match]
      good.link<-links.parsed[grep(match.name,results.tab2$Name)[1]]
      good.link<-paste('http://www.algaebase.org',good.link,sep='')

      details<-GET(good.link)
      details.parsed<-read_html(details)
    }

    classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
    taxa.levels <- html_text(html_nodes(classification.node,"i"))
    if(length(taxa.levels)==0) #no classification data provided in algaebase
    {
      res.df$Empire=
        res.df$Kingdom=
        res.df$Phylum=
        res.df$Class=
        res.df$Order=
        res.df$Family=NA
      return(res.df)
    }
    taxa<-	html_text(html_nodes(classification.node,"a"))
    df<-data.frame(rbind(taxa))
    names(df)<-taxa.levels
    df<-df[,match(groups,names(df))] #make sure there is a consistent set of names
    res.df<-cbind(res.df,df)
  }

  if(res.df$exact.match==1 & res.df$species=='' & res.df$synonyms=='')
  {
    res.df$accepted<-1 ##if genus only match exists, and no synonyms returned,
    #make sure the genus name is flagged as accepted.
  }

  return(res.df)
}
