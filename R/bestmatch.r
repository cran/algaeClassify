#' fuzzy partial matching between a scientific name and a list of possible matches
#'
#' @param enteredName Character string with name to check
#' @param possibleNames Character vector of possible matches
#' @param maxErr maximum number of different bits allowed for a partial match
#' @param trunc TRUE/FALSE. if true and no match, retry with last three letters truncated
#'
#' @export bestmatch
#'
#' @return a character string with the best match, or 'multiplePartialMatches'
#'
#' @examples
#' possibleMatches=c('Viburnum edule','Viburnum acerifolia')
#' bestmatch(enteredName='Viburnum edulus',possibleNames=possibleMatches)

bestmatch=function(enteredName,possibleNames,maxErr=3,trunc=TRUE)
{
  for(i in 0:maxErr)
  {
    match=agrep(enteredName,possibleNames,max.distance=i,value=TRUE)
    
    if(length(match)==1) 
    {
      return(match)
    }
    
    if(length(match)>1)
    {
      if(i==0)
      {
        exact.match=(match==enteredName)
        if(sum(exact.match)==1)
        {
          return(enteredName)
        }
      }else
      {
        return('multiplePartialMatch')
        
      }
    }
  }
  ##strip last three letters and try again
  if(trunc==TRUE)
  {
    len=nchar(enteredName)
    truncName=substr(enteredName,1,len-3)
    trunc=FALSE
    bestmatch(truncName,possibleNames,trunc=trunc)
  }else{
    return(NA) #return NA if no exact or partial match found
  }
}