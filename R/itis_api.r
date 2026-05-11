# Package-level environment holding a persistent curl handle.
# Reusing the same handle lets libcurl keep the TCP+TLS connection to
# www.itis.gov alive across sequential requests (HTTP keep-alive), avoiding
# a fresh handshake on every call.
.itis_pool <- new.env(parent = emptyenv())

# Internal helper: GET one ITIS JSON web service endpoint with an explicit
# timeout. Returns parsed list or NULL on any connectivity/parse error.
# Handles charset detection from Content-Type to cope with Latin-1 encoded
# author names in the ITIS web service responses.
itis_json_get <- function(endpoint, params = list(), timeout = 10) {
  base <- "https://www.itis.gov/ITISWebService/jsonservice/"
  if (length(params) > 0) {
    query <- paste(
      names(params),
      vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), ""),
      sep = "=", collapse = "&"
    )
    url <- paste0(base, endpoint, "?", query)
  } else {
    url <- paste0(base, endpoint)
  }

  # Reuse persistent handle so libcurl keeps the connection alive between calls
  if (!exists("handle", envir = .itis_pool, inherits = FALSE)) {
    .itis_pool$handle <- curl::new_handle()
  }
  h <- .itis_pool$handle
  curl::handle_setopt(h, timeout = timeout, followlocation = 1L)

  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res) || res$status_code != 200L) return(NULL)

  # Detect charset from Content-Type response header (same logic as ritis/crul)
  headers_text <- rawToChar(res$headers)
  ct_lines     <- strsplit(headers_text, "\r\n")[[1]]
  ct_line      <- ct_lines[grepl("content-type", ct_lines, ignore.case = TRUE)]
  enc <- if (length(ct_line) > 0) {
    m <- regmatches(ct_line[1],
                    regexpr("(?i)charset=([^;[:space:]]+)", ct_line[1], perl = TRUE))
    if (length(m) > 0) sub("(?i)charset=", "", m, perl = TRUE) else "UTF-8"
  } else "UTF-8"

  text <- tryCatch(
    iconv(rawToChar(res$content), from = enc, to = "UTF-8", sub = "byte"),
    error = function(e) NULL
  )
  if (is.null(text)) return(NULL)
  tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
}
