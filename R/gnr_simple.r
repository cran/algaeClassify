#' Wrapper function for taxize::gnr_resolve()
#' checks species names against a variety of online databases
#' supports fuzzy partial matching
#'
#' Provides convienent output with a single result, using a variety 
#' of criteria for the best match
#'
#' @param name character string binomial scientific name to resolve
#' @param sourceid integer with data source id from taxize::gnr_datasources()
#' @param topscore boolean. Should the best match be returned based on score?
#' @param numhits boolean. Should the best match be returned based on 
#'        the number of sources with a match?
#' @param canonical If TRUE, names do not include authorship or date
#' @param with_context If TRUE, Match scores are weighted for taxonomic consistency
#' @param ... Other parameters passed to taxize::gnr_resolve()
#'
#' @export gnr_simple
#'
#' @return new data.frame with name matches, column indicating an exact match,
#' scores, and number of hits (matches) from different data sources in gnr_resolve()
#'
#' @examples
#' #use taxize::gnr_datasources() to see all possible data sources for name checking.
#' name<-"Aphanazomenon flos-aquae"
#' #sourceid=3 for ITIS database,195 for Algaebase
#' gnr_simple(name,sourceid=3) #search for ITIS matches
#' gnr_simple(name,sourceid=NULL) #search for matches from any source

gnr_simple<-function(name,sourceid=NULL,topscore=TRUE,numhits=TRUE,canonical=TRUE,
                     with_context=TRUE,...)
{
 
  
	if(length(sourceid)>0) #avoids problems with NA entries.
	{
		if(is.na(sourceid))
		{
			sourceid=NULL
		}
	}
  res<-taxize::gnr_resolve(name,data_source_ids = sourceid,canonical=canonical,
                   with_context = TRUE,...)
  if(length(res)>0)
  {
 
    if(canonical)
    {
      res$matched_name=res$matched_name2
    }
    
    if(topscore)
    {
      top=max(res$score)
      res=res[res$score==top,]
    }else{
      top=NA
    }
    
    if(numhits)
    {
      restab=table(res$matched_name)
      restab<-restab[rev(order(restab))]
      resnames=names(restab)
      maxhits=max(restab)
      resname=resnames[restab==maxhits]
    }else{
      maxhits=NA
      resname=res$matched_name
    }
    resname=unique(resname)[1] #grab the first one if no other options

    if(length(resname)==0)
    {
      resname=NA
    }
    if(is.null(sourceid)){
		sourcename=NA
    }else{
      sources<-taxize::gnr_datasources()
      sourcename=sources$title[sources$id==sourceid]
    }
    
   match=ifelse(resname==name,1,0)
    
    return(data.frame(orig.name=name,exact.match=match,matched.name=resname,numhits=maxhits,score=top,data.source=sourcename))
  }else{
    if(is.null(sourceid)){sourcename=NA
    }else{
      sources<-taxize::gnr_datasources()
      sourcename=sources$title[sources$id==sourceid]
    }
    return(data.frame(orig.name=name,exact.match=0,matched.name=NA,numhits=0,score=0,data.source=sourcename))
  }  
}