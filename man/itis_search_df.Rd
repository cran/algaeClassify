% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itis_search_df.r
\name{itis_search_df}
\alias{itis_search_df}
\title{Wrapper function for applying genus_search_itis and species_search_itis
to a whole data.frame containing scientific names}
\usage{
itis_search_df(df, namecol = NA, higher = FALSE, genus.only = FALSE)
}
\arguments{
\item{df}{data.frame containing names to check}

\item{namecol}{integer or character string with column name containing
species or genus names}

\item{higher}{Boolean. If TRUE, add higher taxonomic classifications to output}

\item{genus.only}{boolean If TRUE, search for matches with just the genus name using genus_search_itis}
}
\value{
data.frame with submitted names (orig.name), matched names (matched.name),
1/0 flag indicating that original name is currently accepted (orig.name.accepted),
1/0 flag indicating if search was genus_only (for distinguishing genus_search_itis
and species_search_itis results), synonyms if any, and higher taxonomy (if
higher=TRUE)
}
\description{
Wrapper function for applying genus_search_itis and species_search_itis
to a whole data.frame containing scientific names
}
\examples{
data(lakegeneva)
#example dataset

new.lakegeneva <- genus_species_extract(lakegeneva[1,],'phyto_name')
new.lakegeneva$genus_species <- trimws(paste(new.lakegeneva$genus,
new.lakegeneva$species))
#checking for genus-only name matches in ITIS, and extracting higher taxonomy
#flagging names with imperfect or no matches

lakegeneva.genus.itischeck <-
         itis_search_df(new.lakegeneva,"genus_species")
         lakegeneva.genus.itischeck
}
