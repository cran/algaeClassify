#' Assign a morphofunctional group based on binary functional traits and higher taxonomy
#'
#' @param flagella 1 if flagella are present, 0 if they are absent.
#' @param size Character string: 'large' or 'small'. Classification criteria is left to the user.
#' @param colonial 1 if typically colonial growth form, 0 if typically unicellular.
#' @param filament 1 if dominant growth form is filamentous, 0 if not.
#' @param centric 1 if diatom with centric growth form, 0 if not. NA for  non-diatoms.
#' @param gelatinous 1 mucilagenous sheath is typically present, 0 if not.
#' @param aerotopes 1 if aerotopes allowing buoyancy regulation are typically present, 0 if not.
#' @param class Character string: The taxonomic class of the species
#' @param order Character string: The taxonomic order of the species
#'
#' @export traits_to_mfg
#' 
#' @return A character string of the species' morphofunctional group
#' 
#' @examples
#' traits_to_mfg(1,"large",1,0,NA,0,0,"Euglenophyceae","Euglenales")
#' 
#' @seealso \url{http://www.algaebase.org} for up-to-date phytoplankton taxonomy,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information

traits_to_mfg <- function(flagella = NA,
                         size = NA,
                         colonial = NA,
                         filament = NA,
                         centric = NA,
                         gelatinous = NA,
                         aerotopes = NA,
                         class = NA,
                         order = NA)
{
  mfg = NA
  if(flagella %in% 1 &
      class %in% c("Bacillariophyceae",
					"Coscinodiscophyceae",
					"Mediophyceae",
					"Fragilariophyceae")==F) {
    #making sure that diatoms are excluded from this branch
    if (order %in% c("Volvocales", 
					  "Chlamydomonadales")){
      if (colonial %in% 1) {
        mfg = "3b-ColoPhyto"
        }
      else {
          mfg = "3a-UnicPhyto"
      }
    }else if(class %in% 'Cryptophyceae'){
    #ensures that all motile cryptophytes go to 2d.
        mfg =  "2d-Crypto"
    }else
      if (size %in% "large") {
        if (class %in% c("Chrysophyceae",
					   "Haptophyceae",
					   "Synurophyceae",
					   "Phaeothamniophyceae")) {
          mfg = "1a-LargeChry"
          }
        else
          if (class %in% "Dinophyceae") {
          mfg = "1b-LargeDino"
          }
          else if (class %in% 'Euglenophyceae'){
            mfg = "1c-LargeEugl"
          }
        }
      else if(size %in% "small"){
        if (class %in% c("Chrysophyceae",
						 "Haptophyceae",
						 "Synurophyceae",
						 "Phaeothamniophyceae")) {
			mfg = "2a-SmallChry1"
          }
        else
          if (class %in% "Dinophyceae"){
            mfg = "2b-SmallDino"
          }
		else
		if (class %in% "Euglenophyceae") {
              mfg = "2c-SmallEugl"
          }
		}
    else {
          mfg = NA
      }
    }
  # first node break: flagella == 0
  else {
    if (class %in% c("Cyanophyceae",
					 "Cyanobacteria")){
      if (colonial %in% 1){
        if (order %in% "Oscillatoriales"){
          mfg = "5a-FilaCyano"
        }
        else
          if (order %in% "Nostocales"){
            mfg = "5e-Nostocales"
          }
        else
          if (size %in% "large") {
            if (aerotopes %in% 1){
              mfg = "5b-LargeVacC"
            }
            else {
              mfg = "5c-OtherChroo"
            }
          }
        else if (size %in% "small"){
          mfg = "5d-SmallChroo"
        }
      }
      else {
        mfg = "4-UnicCyano"
      }
    }
  else
    if (class %in% c("Bacillariophyceae",
				   "Coscinodiscophyceae",
				   "Mediophyceae",
				   "Fragilariophyceae")){
      if (size %in% "large") {
        if (centric %in% 1) {
          if (colonial %in% 1) {
            mfg = "6a1-LColCent"
          }
          else {
            mfg = "6a2-LUniCent"
          }
        }
        else {
          if (colonial %in% 1){
            mfg = "6b1-LColPenn"
          }
        else {
          mfg = "6b2-LUniPenn"
        }
	   }
      }
      else{
        if (centric %in% 1) {
          mfg = "7a-SmallCent"
        }
      else {
        mfg = "7b-SmallPenn"
      }
	 }
    }
  else
    if (colonial %in% 1){
      if (filament %in% 1) {
        if (class %in% c("Chlorophyceae",
						 "Ulvophyceae",
						 "Trebouxiophyceae")){
          mfg = "10a-FilaChlorp"
          }
        else
          if (class %in% c("Conjugatophyceae",
						   "Zygnematophyceae")){
          mfg = "10b-FilaConj"
          }
        else
          if (class %in% c("Xanthophyceae",
						   "Eustigmatophyceae")){
          mfg = "10c-FilaXant"
          }
      }
      else
        if (order %in% c("Chlorococcales",
						"Chlamydomonadales",
						"Tetrasporales")) {
            if (gelatinous %in% 1) {
            mfg = "11b-GelaChlor"
            }
            else {
            mfg = "11a-NakeChlor"
            }
          }
      else {
        mfg = "11c-OtherCol"
      }
        }
      else
    if (size %in% "large") {
      if (class %in% c("Chlorophyceae",
					   "Conjugatophyceae",
					   "Zygnematophyceae")) {
        mfg = "8a-LargeCoCh"
      }
      else {
        mfg = "8b-LargeUnic"
      }
    }
  else {
    if (class %in% c("Conjugatophyceae",
					 "Zygnematophyceae")){
      mfg = "9a-SmallConj"
    }
  else
    if (order %in% "Chlorococcales") {
      mfg = "9b-SmallChlor"
    }
  else
    if (class %in% c("Chrysophyceae",
					 "Synurophyceae",
					 "Phaeothamniophyceae")) {
      mfg = "9c-SmallChry2"
    }
  else {
    mfg = "9d-SmallUnic"
  }
  }
 }
  return(mfg)
}
