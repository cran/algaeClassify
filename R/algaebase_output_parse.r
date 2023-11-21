#' Helper function for parsing output from algaebase
#' @param x list object containing output from an algaebase query
#' @param field.name character string
#'
#' @export algaebase_output_parse
#'
#' @return selected output variable as character vector
algaebase_output_parse<-function(x,field.name) {
  
  res<-x[[field.name]]
  if(is.null(res)){res=NA}
  return(res)
}