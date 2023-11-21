#' Wrapper function to apply gnr_simple across a data.frame or list of species names
#'
#' Provides convienent output with a row per name. To streamline merging with 
#' original data.
#'
#' @param df data.frame containing names to check
#' @param name.column integer or character string with column name containing 
#' 		  species names
#' @param sourceid integer with data source id from taxize::gnr_datasources()
#' @param topscore boolean. Should the best match be returned based on score?
#' @param numhits boolean. Should the best match be returned based on 
#'        the number of sources with a match?
#' @param canonical If TRUE, names do not include authorship or date
#' @param with_context If TRUE, Match scores are weighted for taxonomic consistency
#' @param ... Other parameters passed to taxize::gnr_resolve()
#'
#' @export gnr_simple_df
#'
#' @return new data.frame original names (orig.name), 1/0 flag for an exact match,
#' 			the best match (matched.name), and other output from gnr_simple().
#' 			scores, and number of hits (matches) from different data sources in 
#'			gnr_resolve()
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' new.lakegeneva$genus_species <- trimws(paste(new.lakegeneva$genus,
#'	new.lakegeneva$species))
#'
#' #checking for matches from all GNRS sources:
#' lakegeneva.namematches <- gnr_simple_df(new.lakegeneva[1:10,],"genus_species")
#' lakegeneva.namematches


gnr_simple_df<-function(df,name.column,sourceid=NA,topscore=TRUE,numhits=TRUE,canonical=TRUE,with_context=TRUE,...)
{
  nrows<-nrow(df)
  gnr.df<-data.frame("orig.name"=NULL,"exact.match"=NULL,"matched.name"=NULL,"numhits"=NULL,"score"=NULL,"data.source"=NULL) 
  
    for(i in 1:nrows)
    {
        name=df[[name.column]][i]
        if(!is.na(name) & !is.null(name))
        {
          tmp<-gnr_simple(name,sourceid = sourceid,topscore = topscore,
                        numhits = numhits,canonical = canonical,...)
        }else{
          tmp<-data.frame("orig.name"=NA,"exact.match"=NA,"matched.name"=NA,"numhits"=NA,"score"=NA,"data.source"=NA) 
        }
        gnr.df<-rbind(gnr.df,tmp)
    }
  return(gnr.df)
  
}
