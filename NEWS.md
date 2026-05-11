# algaeClassify 2.0.6

## Bug fixes and refactoring

- Rewrote `genus_search_itis()`, `species_search_itis()`, and `itis_search_df()` to
  query the ITIS JSON web service (`www.itis.gov/ITISWebService/jsonservice/`) directly,
  replacing all `ritis::` function calls. The `ritis` and `RCurl` packages are no longer
  required and have been removed from `Imports`.
- Added internal helper `itis_json_get()` (not exported) that wraps `curl::curl_fetch_memory()`
  with an explicit timeout, charset-aware response decoding (handles Latin-1 author names
  in ITIS responses), and a package-level persistent curl handle (`.itis_pool`) that
  reuses the TCP/TLS connection to `www.itis.gov` across sequential requests.
- Fixed higher-taxonomy and synonym lookups when the matched name is a synonym or
  orthographic variant. Both `genus_search_itis()` and `species_search_itis()` now
  retrieve `acceptedTsn` from ITIS and use it for hierarchy and synonym lookups.
  Previously, synonym TSNs had no parent in the ITIS hierarchy, so these lookups
  returned NA even when the accepted name was correctly resolved (e.g.
  `"Aphanizomenon flosaquae"` → `"Aphanizomenon flos-aquae"`, genus `"Cyanarcus"` →
  `"Gomontiella"`).
- Fixed hyphen handling in `species_search_itis()`: hyphens in species epithets are
  now preserved (e.g. `"Aphanizomenon flos-aquae"` resolves correctly instead of
  returning `matched.name=NA`).
- Added comprehensive `tryCatch` error handling to all ITIS API calls in
  `genus_search_itis()` and `species_search_itis()`: any connectivity or parse failure
  returns a graceful NA-padded result with a warning rather than crashing.
- Wrapped `\examples{}` for `genus_search_itis()`, `species_search_itis()`, and
  `itis_search_df()` in `\donttest{}`. These examples require a live ITIS network
  connection; elapsed time depends on server response, not package code.

# algaeClassify 2.0.5

## Major changes

- Version 2.0.x includes tools for querying the algaebase taxonomic database with a valid API key.
- Version 1.4.x includes alternative tools for standardizing taxonomic names
and extracting higher taxonomy using the Geographic Names Resolution Service and 
the ITIS taxonomic database.

## Minor changes

-Version 2.0.1 has been approved as an official U.S. Geological Survey software release.
-Version 2.0.2 includes improved error handling for itis search functions, and will return
a warning and data.frame containing NAs if the ITIS website is temporarily down.
-Version 2.0.3 queries the Global Names Resolver via its API, rather than calling
  functions from the taxize package.
-Version 2.0.4 contains revised functions for querying the Global Names Resolver based on its updated API.
-Version 2.0.5 contains bug fixes for the gnr_simple() and gnr_df() search functions.
