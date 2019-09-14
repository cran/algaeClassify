# algaeClassify

<!-- badges: start -->
<!-- badges: end -->

The goal of algaeClassify is to facilitate the analysis of taxonomic and functional trait
data for phytoplankton.

## Installation

You can install the released version of algaeClassify from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("algaeClassify")
```

The development version can be installed from github with:
``` r
library(devtools)
install_github("vppatil/GEISHA_phytoplankton/package builds/algaeClassify",ref="working")
```

## Example

This is a basic example which shows you how to use algaeClassify to 
1) identify anomalies in a time-series of phytoplankton species
2) verify/correct species names using algaebase
3) calculate aggregate abundance at a higher taxonomic level (genus)
4) re-plot species accumulation curves to see if the taxonomic standardization and 
aggregation to higher taxonomy have resolved the anomalies.

``` r
library(algaeClassify)

data(lakegeneva) #load a demonstration dataset

#view species accumulation curve over duration of dataset to check for anomalies
accum(lakegeneva,phyto_name='genus',column='biovol_um3_ml',n=100,datename='date_dd_mm_yy',dateformat='%d-%m-%y')

#clean up binomial names and extract genus and species to new columns
lakegeneva<-genus_species_extract(lakegeneva,phyto.name='phyto_name')

#compare names against accepted taxonomy in algaebase, and extract higher taxonomy
lakegeneva.algaebase<-spp_list_algaebase(lakegeneva,long=TRUE,write=FALSE)

#merge taxonomic information into the original database
lakegeneva<-merge(lakegeneva,lakegeneva.algaebase)

#aggregate abundance data to genus level
lakegeneva.genus<-phyto_ts_aggregate(lakegeneva,SummaryType='abundance',AbundanceVar='biovol_um3_ml',
                    GroupingVar1='genus')

#plot accumulation curve again, but at genus level
accum(lakegeneva.genus,phyto_name='genus',column='biovol_um3_ml',n=100,datename='date_dd_mm_yy',dateformat='%Y-%m-%d')




```

