% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traits_to_csr_df.r
\name{traits_to_csr_df}
\alias{traits_to_csr_df}
\title{Add CSR functional group classifications to a dataframe of phytoplankton species, based on surface to volume ratio and
maximum linear dimension ranges proposed by Reynolds et al. 1988;2006}
\usage{
traits_to_csr_df(
  df,
  sav,
  msv,
  msv.source = "Reynolds 2006",
  traitrange = algaeClassify::traitranges
)
}
\arguments{
\item{df}{name of dataframe}

\item{sav}{character string with name of column that contains surface to volume ratio values}

\item{msv}{character string with name of column that contains maximum linear dimension * surface to volume ratio values}

\item{msv.source}{character string with reference source for distinguishing criteria}

\item{traitrange}{data frame with trait criteria for c,s,r groups. The included table
can be replaced with user-defined criteria if desired. Measurements are:
Surface area/volume ratio (sav), maximum linear dimension (mld) and mld*sav (msv).}
}
\value{
a character string with one of 5 return values: C,CR,S,SR, or R
}
\description{
Add CSR functional group classifications to a dataframe of phytoplankton species, based on surface to volume ratio and
maximum linear dimension ranges proposed by Reynolds et al. 1988;2006
}
\examples{

csr.df<-data.frame(msv=10,sav=1)

csr.df$CSR<-traits_to_csr_df(csr.df,'msv','sav')

print(csr.df)
}
