% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mfg_csr.r
\name{mfg_csr_convert}
\alias{mfg_csr_convert}
\title{Returns a CSR classification based on Morphofunctional group (MFG).
Correspondence based on Salmaso et al. 2015 and Reynolds et al. 1988}
\usage{
mfg_csr_convert(mfg)
}
\arguments{
\item{mfg}{Character string with MFG name, following Salmaso et al. 2015}
}
\value{
A character string with values 'C','S','R','CR','SC','SR', or NA
}
\description{
Returns a CSR classification based on Morphofunctional group (MFG).
Correspondence based on Salmaso et al. 2015 and Reynolds et al. 1988
}
\examples{

mfg_csr_convert("11a-NakeChlor")
}
