#' Assign morphofunctional groups to a dataframe of functional traits and higher taxonomy
#'
#' @param dframe An R dataframe containing functional trait information and higher taxonomy
#' @param arg.names Character string of column names corresponding to arguments for traits_to_mfg()
#' @return A character vector containing morpho-functional group (MFG) designations
#'
#' @export traits_to_mfg_df
#'
#' @examples
#' #create a two-row example dataframe of functional traits
#' func.dframe=data.frame(flag=1,size=c("large","small"),col=0,fil=0,cent=NA,gel=0,
#'                        aer=0,cl="Euglenophyceae",or="Euglenales",stringsAsFactors=FALSE)
#'                        
#' #check the dataframe                       
#' print(func.dframe)                        
#'
#' #run the function to produce a two-element character vector
#' traits_to_mfg_df(func.dframe,c("flag","size","col","fil","cent","gel","aer","cl","or"))
traits_to_mfg_df<-function(dframe,arg.names=c("flagella",
                                              "size",
                                              "colonial",
                                              "filament",
                                              "centric",
                                              "gelatinous",
                                              "aerotopes",
                                              "class",
                                              "order"))
{ 
  #dframe is a data frame of functional traits
  #arg.names is a vector of column names that match the arguments for traits_to_mfg()
  mfg.from.traits=""
  for(i in 1:dim(dframe)[1])
  {
    mfg.from.traits[i]=traits_to_mfg(dframe[[arg.names[1]]][i],
                                     dframe[[arg.names[2]]][i],
                                     dframe[[arg.names[3]]][i],
                                     dframe[[arg.names[4]]][i],
                                     dframe[[arg.names[5]]][i],
                                     dframe[[arg.names[6]]][i],
                                     dframe[[arg.names[7]]][i],
                                     dframe[[arg.names[8]]][i],
                                     dframe[[arg.names[9]]][i])
  }
  
  return(mfg.from.traits)
}
