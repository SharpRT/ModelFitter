
library(NFRRPhilippines)

rm(list=ls())

#Internal structure of extdata = Philippines/Field/[Year]/[Season]/[Institute]_[Site]/Week_[i]/Site_Week[i]_Plot[plotType]_[countType].csv
##Where Institute= "IRRI" if site = "Pila" or "Victoria"; and, "PRRI" if site = "Malayantoc" or "Maligaya" 
##Where plotType = "Res" or "Sus" and countType = "I" or "N".
##These csv files then contain a spatially resolved grid of the 40x10 hills.
##Also in the Field directory are two additional data files SampleDates.csv and Final_N_Week.csv.

dataPath = "D:/_temp/extdata/"

site = "Pila" #Pila, Victoria, Malayantoc, Maligaya
year = 2016 #2016, 2017, 2018
season = "Wet" #Wet, Dry

fitval = fitOut_philippinesStandard(year, season, site, filepath=dataPath)
  
summary(fitval)
