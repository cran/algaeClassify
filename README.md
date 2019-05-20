<!-- README.md is generated from README.Rmd. Please edit that file -->
algaeClassify
=============

The goal of this package is to facilitate the assignment of morpho-functional group (MFG) classifications to phytoplankton species. MFG classifications are based on a combination of taxonomy (Class,Order) and a suite of 7 binomial functional traits. Classifications are derived from Salmaso, Nico, Luigi Naselli-Flores, and Judit Padisak. "Functional classifications and their application in phytoplankton ecology." Freshwater Biology 60.4 (2015): 603-619, and this reference should be cited when using the package. The algaeClassify package is a product of the GEISHA (Global Evaluation of the Impacts of Storms on freshwater Habitat and Structure of phytoplankton Assemblages), funded by CESAB (the Centre for Synthesis and Analysis of Biodiversity) and the USGS John Wesley Powell Center, with data and other support provided by members of GLEON (the Global Lake Ecology Observation Network). This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

Example
-------

The two functions in this package allow you to assign MFg classifications to 1) a single taxon or 2) a dataframe of phytoplankton species.

``` r
library(algaeClassify)
## using traits_to_mfg to assign MFg to a single species:
traits_to_mfg(1,"large",1,0,NA,0,0,"Euglenophyceae","Euglenales")
#> [1] "1c-LargeEugl"

## using traits_to_mfg_df
#first, create a two-row example dataframe of functional traits
func.dframe=data.frame(flag=1,size=c("large","small"),col=0,fil=0,cent=NA,gel=0,
                        aer=0,cl="Euglenophyceae",or="Euglenales",stringsAsFactors=FALSE)
#check the dataframe                       
func.dframe                       
#>   flag  size col fil cent gel aer             cl         or
#> 1    1 large   0   0   NA   0   0 Euglenophyceae Euglenales
#> 2    1 small   0   0   NA   0   0 Euglenophyceae Euglenales

#run the function to produce a two-element character vector
traits_to_mfg_df(func.dframe,c("flag","size","col","fil","cent","gel","aer","cl","or"))
#> [1] "1c-LargeEugl" "2c-SmallEugl"
```
