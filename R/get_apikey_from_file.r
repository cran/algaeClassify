#' Get value of algaebase API key from a file 
#' @param keyfile path to text file
#'
#' @export get_apikey_fromfile
#' @return api key as character string (invisibly)
#'
#' @examples \dontrun{apikey<-get_apikey_fromfile("keyfile.txt")}

get_apikey_fromfile<-function(keyfile)
{
  #keyfile should be a text file with the API key on the first line
  apikey<-readLines(con=keyfile,n=1,warn=FALSE)
  invisible(apikey) #will invisibly return result
  
}
