#' Add algaebase API key to curl handle
#' @param apikey character string with valid key 
#'
#' @export set_algaebase_apikey_header
#'
#' @return curl handle object
set_algaebase_apikey_header<-function(apikey=NULL)
{
  if(is.null(apikey)){
    stop("You must have a valid API key for www.algaebase.org")
  }
  h <- curl::new_handle(verbose = TRUE)
  curl::handle_setheaders(h,
                    "Content-Type" = "application/json",
                    "abapikey" = apikey)
  return(h)
}