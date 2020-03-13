###############################################################Creates a A spectral library object of all the scans passed in########################################################
library(spectrolab)
library(tidyverse)

#params:
##vectorOfSampleDirectories: list of strings. each string is the directory to a saved spectra R object
#
#output: spectralLibrary R Object

createSpectralLibrary <- function(vectorOfSampleDirectories) {
  tryCatch({
    listOfSpectra <- list()
    index <- 1
    
    ##Reads in a spectral object for each given directory...all object have bands from 350:2500nm and metadata being ScanaID,PFT and Area
    for (directory in vectorOfSampleDirectories) {
      listOfSpectra[[index]] <- readRDS(directory)
      index <- index + 1
    }
    
    ##This function combines the list of spectral objects above
    spectralLibrary<-Reduce(spectrolab::combine,listOfSpectra)
    
    ##Now we want to convert our spectral object to a dataframe to build our spectral library
    spectralLibrary<-as.data.frame(spectralLibrary)%>%dplyr::select(-sample_name)
    
    ##Now we want to add columns that represent the species and funcional groups, PFT_2 and PFT_3 respectively. 
    ##Add column PFT_2 (SPECIES) to spectral library
    spectralLibrary$PFT_2[spectralLibrary$PFT=="abibal"]<-"Abies balsamea"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="acerub"]<-"Acer rubrum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="acepen"]<-"Acer pensylvanicum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="aleoch"]<-"Alectoria ochroleuca"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="alnfru"]<-"Alnus sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="alninc"]<-"Alnus Incana"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="arccen"]<-"Arctocetraria centrifuga"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="arcnig"]<-"Arctostaphyllos"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="arcrub"]<-"Arctostaphyllos"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="arcsta"]<-"Arctostaphyllos"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="arctop"]<-"Unknown"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="asachr"]<-"Asahinea chrysantha"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="aulpal"]<-"Aulacomnium palustre"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="aultur"]<-"Aulacomnium turgidum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="bare rock"]<-"Bare Rock"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="bare_soil"]<-"Bare Soil"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="betall"]<-"Betula alleghaniensis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="betnan"]<-"Betula nana"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="betneo"]<-"Betula neoalaskana"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="betpap"]<-"Betula papyrifera"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="betpop"]<-"Betula populifolia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="bryoria"]<-"Bryoria sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="calcan"]<-"Calamogrostis sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="carlin"]<-"Carex sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="carlyn"]<-"Carex sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="carram"]<-"Carex sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="castet"]<-"Cassiope tetragona"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="cerpur"]<-"Ceratadon purpureus"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="cetisl"]<-"Cetraria islandica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="cetlae"]<-"Cetraria laevigata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="claama"]<-"Cladonia amaurocraea"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clacor"]<-"Cladonia cornuta"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clacuc"]<-"Flavocetraria cucculata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clagra"]<-"Cladonia gracilis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clamit"]<-"Cladonia mitis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="claran"]<-"Cladonia rangiferina"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="claste"]<-"Cladonia steallaris"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clasty"]<-"Cladonia stygia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="clasul"]<-"Cladonia sulphurina"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="claunc"]<-"Cladonia uncialis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dacarc"]<-"Dactylina arctica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dead salix"]<-"Dead Salix"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dicranum"]<-"Dicranum sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dryala"]<-"Dryas alleghenies"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dryhyb"]<-"Dryas sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="dryoct"]<-"Dryas octopetala"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="empnig"]<-"Empetrum nigrum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="equarv"]<-"Equisetum arvense"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="equsyl"]<-"Equisetum sylvaticum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="erivag"]<-"Eriophorum vaginatum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="evemes"]<-"Evernia mesomorpha"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="faggra"]<-"Fagus grandifolia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="flacuc"]<-"Flavocetraria cucculata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="flaniv"]<-"Flavocetraria nivalis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="fraame"]<-"Fraxinus americana"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="gravel"]<-"Gravel"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="herlan"]<-"Heracleum lanatum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="hylspl"]<-"Hylocomium splendens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="hypaus"]<-"Hypogymnia austerodes"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="hypspl"]<-"Hylocomium splendens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="icmeri"]<-"Icmadophila ericetorum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="irisit"]<-"Iris sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="larlar"]<-"Larix Larcina"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="leddec"]<-"Ledum decumbens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="loipro"]<-"Loisleuria procumbens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="luparc"]<-"Lupinus sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="masric"]<-"Masonhalea richardsonii"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="melanelia"]<-"Melanelia sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="melhep"]<-"Melanelia sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="neparc"]<-"Nephroma arcticum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="naparc"]<-"Nephroma arcticum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="orange_Porpidia"]<-"Porpidia sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="paramb"]<-"Parmeliopsis ambigua"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="paromp"]<-"Parmelia omphalodes"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="parsul"]<-"Parmelis sulcata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pedrac"]<-"Pedicularis racemosa"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pedsud"]<-"Pedicularis sudetica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pelapt"]<-"Peltigera apthosa"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pelleu"]<-"Peltigers leucophlebia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pelmal"]<-"Peltigera malacea"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pelsca"]<-"Peltigera scabrata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="petfri"]<-"Petasites frigida"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="picmar"]<-"Picea mariana"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="picrub"]<-"Picea rubens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pilaci"]<-"Pilophorus acicularis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="pinstr"]<-"Pinus strobus"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="plagiomnium"]<-"Plagiomnium sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="plesch"]<-"Pleurozium schreberi"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="poljen"]<-"Polytrichum juniperinum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="poljun"]<-"Polytrichum juniperinum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="polstr"]<-"Polytrichum strictum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="polytrichum"]<-"Polytrichum sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="popbal"]<-"Populus balsamifera"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="popgra"]<-"Populus grandidentata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="prupen"]<-"Prunus pensylvanica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="quartz"]<-"Quartz"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="querub"]<-"Quercus Rubra"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="raclan"]<-"Racomitrium lanoiginosum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rhutyp"]<-"Rhus typhina"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rhyrug"]<-"Rhytidum rugosum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rosaci"]<-"Rosa acicularis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rosasc"]<-"Rosa acicularis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rubcam"]<-"Rubus sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="rubcha"]<-"Rubus sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salala"]<-"Salix alaxensis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salarb"]<-"Salix arbusculoides"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salgla"]<-"Salix glauca"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="sallan"]<-"Salix lanata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salova"]<-"Salix ovalifolia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="Salova"]<-"Salix ovalifolia"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salpul"]<-"Salix pulchra"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="salric"]<-"Salix richardsonii"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="sphagn"]<-"Sphagnum sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="sphfus"]<-"Sphagnum fuscum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="spruce bark"]<-"Pices (bark)"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="stepas"]<-"Stereocaulon sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="stetas"]<-"Stereocaulon sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="thuocc"]<-"Thuja occidentalis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="toefeldia"]<-"Toefeldia sp."
    spectralLibrary$PFT_2[spectralLibrary$PFT=="tomnit"]<-"Tomenthypnum nitens"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="tragra"]<-"Trapelopsis granulosa"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="tsucan"]<-"Tsuga canadensis"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="umbarc"]<-"Umbilicaria arctica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="usnlap"]<-"Usnea lapponica"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="usnsca"]<-"Usnea scabrata"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="vacvit"]<-"Vaccinium vitis-idea"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="vaculi"]<-"Vaccinium uliginosum"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="vulpin"]<-"Vulpicida pinastri"
    spectralLibrary$PFT_2[spectralLibrary$PFT=="wooly_salix"]<-"Salix (wooly)"
    
    ###Add column PFT_3 (Courser response variables)
    spectralLibrary$PFT_3[spectralLibrary$PFT=="abibal"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="acerub"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="acepen"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="aleoch"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="alnfru"]<-"Shrub_Alder"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="alninc"]<-"Shrub_Alder"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="arccen"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="arcnig"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="arcrub"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="arcsta"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="arctop"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="asachr"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="aulpal"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="aultur"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="bare rock"]<-"Abiotic_Rock"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="bare_soil"]<-"Abiotic_Soil"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="betall"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="betnan"]<-"Shrub_Other"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="betneo"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="betpap"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="betpop"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="bryoria"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="calcan"]<-"Graminoid_Grass"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="carlin"]<-"Graminoid_Sedge"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="carlyn"]<-"Graminoid_Sedge"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="carram"]<-"Graminoid_Sedge"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="castet"]<-"Dwarf_Shrub_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="cerpur"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="cetisl"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="cetlae"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="claama"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clacor"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clacuc"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clagra"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clamit"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="claran"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="claste"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clasty"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="clasul"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="claunc"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dacarc"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dead salix"]<-"Abiotic_Litter"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dicranum"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dryala"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dryhyb"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="dryoct"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="empnig"]<-"Dwarf_Shrub_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="equarv"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="equsyl"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="erivag"]<-"Graminoid_Sedge"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="evemes"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="faggra"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="flacuc"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="flaniv"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="fraame"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="gravel"]<-"Abiotic_Rock"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="grey_rhizocarpon"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="herlan"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="hylspl"]<-"Moss_Pleurocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="hypaus"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="hypspl"]<-"Moss_Pleurocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="icmeri"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="irisit"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="larlar"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="leddec"]<-"Shrub_Other"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="loipro"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="luparc"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="masric"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="melanelia"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="melhep"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="neparc"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="naparc"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="orange_Porpidia"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="paramb"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="paromp"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="parsul"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pedrac"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pedsud"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pelapt"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pelleu"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pelmal"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pelsca"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="petfri"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="picmar"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="picrub"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pilaci"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="pinstr"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="plagiomnium"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="popbal"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="popgra"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="prupen"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="plesch"]<-"Moss_Pleurocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="poljen"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="poljun"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="polstr"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="polytrichum"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="popbal"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="quartz"]<-"Abiotic_Rock"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="querub"]<-"Tree_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="raclan"]<-"Moss_Pleurocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rhigeo"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rhyrug"]<-"Moss_Acrocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rhutyp"]<-"Shrub_Other"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rosaci"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rosasc"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rubcam"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="rubcha"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salala"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salarb"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salgla"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="sallan"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salova"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="Salova"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salpul"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="salric"]<-"Shrub_Salix"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="sphagn"]<-"Moss_Sphagnum"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="sphfus"]<-"Moss_Sphagnum"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="spruce bark"]<-"Abiotic_Litter"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="stepas"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="stetas"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="thuocc"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="toefeldia"]<-"Forb"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="tomnit"]<-"Moss_Pleurocarp"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="tragra"]<-"Lichen_Light"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="tsucan"]<-"Tree_Needle"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="umbarc"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="umbhyp"]<-"Lichen_Dark"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="usnlap"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="usnsca"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="vacvit"]<-"Shrub_Other"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="vaculi"]<-"Dwarf_Shrub_Broad"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="vulpin"]<-"Lichen_Yellow"
    spectralLibrary$PFT_3[spectralLibrary$PFT=="wooly_salix"]<-"Shrub_Salix"
    
    ##Lets add more details to our spectral library by adding  frequency columns
    ##These frequency values represent the number of scans per species and the number of scans per functional group
    ##Lets start by creating a new dataframe with a frequency column for species and one for functional group
    ##Where PFT_2 represents species and PFT_3 represents functional groups
    spectralLibraryPFT2_freqTab<-as.data.frame(table(spectralLibrary$PFT_2))##SPECIES FREQ TABLE
    spectralLibraryPFT3_freqTab<-as.data.frame(table(spectralLibrary$PFT_3))##Func Group FREQ TABLE
    
    ##Lets combine the frequency table with our spectral library...this adds two more columns to our spectral library
    spectralLibrary$Freq1<-spectralLibraryPFT2_freqTab$Freq[match(spectralLibrary$PFT_2,spectralLibraryPFT2_freqTab$Var1)]
    spectralLibrary$Freq2<-spectralLibraryPFT3_freqTab$Freq[match(spectralLibrary$PFT_3,spectralLibraryPFT3_freqTab$Var1)]
    
    ##Lets reorder the columns so our data is a little more structured. 
    spectralLibrary<-spectralLibrary%>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,Freq1,Freq2,everything())
    
    ##Lets remove the 5 scans that were unknown
    spectralLibrary<-subset(spectralLibrary,PFT_3!="Unknown")
    
    ##Lets remove abiotic group as this is a very broad category
    spectralLibrary<-subset(spectralLibrary,PFT_3!="Abiotic")
    
    ###Lets remove all the uncalibrated scans, but first we have to identify those scans
    ##Checck to see if there are any values greater than 2 or less than 0
    tst<-lapply(spectralLibrary[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
    tst$V1%>%range()
    tst$V2%>%range()##Values here exceed 2, so we should remove them
    tst<-subset(tst,V2>2)
    #tst%>%View()
    
    ##now we have all the columns that have values greater than two, lets save those column names in an object
    ##TODO: We could probably create a better function here to extract those rows with values >2
    ##Here I created an object with all the columns that had values <2 or >0
    badscans<-rownames(tst)
    badscans<-c("1891"  ,"1892" ,"1893" ,"1894" ,"1895" ,"1896" ,"1897" ,"1898" ,"1899" ,"1900" ,"1901" ,"1902" ,"1908" ,"1909"
                ,"1916" ,"1917" ,"1930" ,"1931" ,"1932" ,"1938" ,"1939" ,"1940" ,"1947" ,"1948" ,"2480" ,"2481" ,"2482" ,"2490"
                ,"2491" ,"2492" ,"2493" ,"2497" ,"2498" ,"2499" ,"2500")
    
    
    ##Column names are saved, lets create a function that will will remove all those rows that have values greater than 2
    ####Need to come up with a better function
    spectralLibrary<- spectralLibrary[apply(spectralLibrary[,badscans]<2, 1, all),]### dim 1917 2158, there were 5 rows with values >2
    
    ###lets create a df from our spectral library to be saved
    spectralLibrary_df<-spectralLibrary

     #We want to create dataframes that have all the scans of each functional groups
    ##This can be used to make graphs of all the species within each functional group
    spectralLibrary_Lichen    <-subset(spectralLibrary,PFT_3=="Lichen")
    spectralLibrary_Tree      <-subset(spectralLibrary,PFT_3=="Tree")
    spectralLibrary_Dwarfshrub<-subset(spectralLibrary,PFT_3=="Dwarf Shrub")
    spectralLibrary_shrub     <-subset(spectralLibrary,PFT_3=="Shrub")
    spectralLibrary_Moss      <-subset(spectralLibrary,PFT_3=="Moss")
    spectralLibrary_Graminoid <-subset(spectralLibrary,PFT_3=="Graminoid")
    spectralLibrary_Forb      <-subset(spectralLibrary,PFT_3=="Forb")
    
    ##Now we want to convert our new spectral library back to a spectral object to be saved
    #First Remove metadata from spectral library
    spectralLibrary_meta<-spectralLibrary[,c(1:7)]
    
    ###Create alskaspeclib without meta
    spectralLibrary_spectra<-spectralLibrary[,c(-1:-7)]
    
    ##convert to a .rds file
    spectralLibrary<-spectrolab::as.spectra(spectralLibrary_spectra)
    ##str(spectralLibrary_spectra)
    
    ##bind metadata
    meta(spectralLibrary)<-data.frame(spectralLibrary_meta, stringsAsFactors = FALSE)
    
    ##Now lets smooth and resample our new spectral library so we can perform other analysis later
    spectralLibrary_smooth<-smooth(spectralLibrary)
    
    #resampling every 10, 50 and 100nm
    spectralLibrary_smooth_010nm<-spectrolab::resample(spectralLibrary_smooth, seq (350,2500,10 ))
    spectralLibrary_smooth_050nm<-spectrolab::resample(spectralLibrary_smooth, seq (350,2500,50))
    spectralLibrary_smooth_100nm<-spectrolab::resample(spectralLibrary_smooth, seq (350,2500,100))
    
    ##Lets create a df were all each functional group has the same number of scans, in this case 25.
    ##First lets convert the spectral objects to dataframes
    spectralLibrary_smooth_equal25      <-spectralLibrary_smooth      %>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_smooth_010nm_equal25<-spectralLibrary_smooth_010nm%>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_smooth_050nm_equal25<-spectralLibrary_smooth_050nm%>%as.data.frame()%>%dplyr::select(-sample_name)
    spectralLibrary_smooth_100nm_equal25<-spectralLibrary_smooth_100nm%>%as.data.frame()%>%dplyr::select(-sample_name)
    
    ##Now we can create those dataframes
    spectralLibrary_smooth_equal25      <-spectralLibrary_smooth_equal25      %>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
    spectralLibrary_smooth_010nm_equal25<-spectralLibrary_smooth_010nm_equal25%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
    spectralLibrary_smooth_050nm_equal25<-spectralLibrary_smooth_050nm_equal25%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
    spectralLibrary_smooth_100nm_equal25<-spectralLibrary_smooth_100nm_equal25%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
    
    #for now we just return the spectralLibrary object becuase we arnt using the other objects created
    return(spectralLibrary)
  }, warning = function(warning) {
    message <- paste("WARNING - While creating specral library", warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While while creating spectral library", error, sep = " : ")
    stop(message)
  })
}
