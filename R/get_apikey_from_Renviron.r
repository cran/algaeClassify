#' Get value of algaebase API key from Environment variable
#' Return an error if variable not set.
#'
#' @export get_apikey
#' @return api key as character string (invisibly)

get_apikey <- function() {
  val <- Sys.getenv("ALGAEBASE_APIKEY")
  if (identical(val, "")) {
    stop("`ALGAEBASE_APIKEY` env var has not been set. Add it to your ~/.Renviron file")
  }
  invisible(val)
}