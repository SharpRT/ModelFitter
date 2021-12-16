
library(NFRRPhilippines)

rm(list=ls())

dataPath = "D:/_temp/extdata/"
tColStr = "DAT" #DAT recommended.

getPlotSum = function(site, year, season, plotType){
    plotSum = calc_sumPlotWeek(country="Philippines", experiment="Field", site=site, year=year, season=season, sumVar="numTillers", filepath = dataPath)
    plotSum = plotSum[plotSum$type=="I",]
    plotSum = plotSum[plotSum$Plot_Type==plotType,]
    return(plotSum)
}

ps_16wetMaliRes = getPlotSum(year=2016, season="Wet", plotType="res", site="Maligaya")
ps_16wetMaliSus = getPlotSum(year=2016, season="Wet", plotType="sus", site="Maligaya")
ps_16wetMalaRes = getPlotSum(year=2016, season="Wet", plotType="res", site="Malayantoc")
ps_16wetMalaSus = getPlotSum(year=2016, season="Wet", plotType="sus", site="Malayantoc")
ps_17dryMaliRes = getPlotSum(year=2017, season="Dry", plotType="res", site="Maligaya")
ps_17dryMaliSus = getPlotSum(year=2017, season="Dry", plotType="sus", site="Maligaya")
ps_17dryMalaRes = getPlotSum(year=2017, season="Dry", plotType="res", site="Malayantoc")
ps_17dryMalaSus = getPlotSum(year=2017, season="Dry", plotType="sus", site="Malayantoc")
ps_17wetMaliRes = getPlotSum(year=2017, season="Wet", plotType="res", site="Maligaya")
ps_17wetMaliSus = getPlotSum(year=2017, season="Wet", plotType="sus", site="Maligaya")
ps_17wetMalaRes = getPlotSum(year=2017, season="Wet", plotType="res", site="Malayantoc")
ps_17wetMalaSus = getPlotSum(year=2017, season="Wet", plotType="sus", site="Malayantoc")
ps_18dryMaliRes = getPlotSum(year=2018, season="Dry", plotType="res", site="Maligaya")
ps_18dryMaliSus = getPlotSum(year=2018, season="Dry", plotType="sus", site="Maligaya")
ps_18dryMalaRes = getPlotSum(year=2018, season="Dry", plotType="res", site="Malayantoc")
ps_18dryMalaSus = getPlotSum(year=2018, season="Dry", plotType="sus", site="Malayantoc")
ps_18wetMaliRes = getPlotSum(year=2018, season="Wet", plotType="res", site="Maligaya")
ps_18wetMaliSus = getPlotSum(year=2018, season="Wet", plotType="sus", site="Maligaya")
ps_18wetMalaRes = getPlotSum(year=2018, season="Wet", plotType="res", site="Malayantoc")
ps_18wetMalaSus = getPlotSum(year=2018, season="Wet", plotType="sus", site="Malayantoc")

ps_16wetPilaRes = getPlotSum(year=2016, season="Wet", plotType="res", site="Pila")
ps_16wetPilaSus = getPlotSum(year=2016, season="Wet", plotType="sus", site="Pila")
ps_16wetVictRes = getPlotSum(year=2016, season="Wet", plotType="res", site="Victoria")
ps_16wetVictSus = getPlotSum(year=2016, season="Wet", plotType="sus", site="Victoria")
ps_17wetPilaRes = getPlotSum(year=2017, season="Wet", plotType="res", site="Pila")
ps_17wetPilaSus = getPlotSum(year=2017, season="Wet", plotType="sus", site="Pila")
ps_17wetVictRes = getPlotSum(year=2017, season="Wet", plotType="res", site="Victoria")
ps_17wetVictSus = getPlotSum(year=2017, season="Wet", plotType="sus", site="Victoria")
ps_18dryPilaRes = getPlotSum(year=2018, season="Dry", plotType="res", site="Pila")
ps_18dryPilaSus = getPlotSum(year=2018, season="Dry", plotType="sus", site="Pila")
ps_18dryVictRes = getPlotSum(year=2018, season="Dry", plotType="res", site="Victoria")
ps_18dryVictSus = getPlotSum(year=2018, season="Dry", plotType="sus", site="Victoria")
ps_18wetPilaRes = getPlotSum(year=2018, season="Wet", plotType="res", site="Pila")
ps_18wetPilaSus = getPlotSum(year=2018, season="Wet", plotType="sus", site="Pila")
ps_18wetVictRes = getPlotSum(year=2018, season="Wet", plotType="res", site="Victoria")
ps_18wetVictSus = getPlotSum(year=2018, season="Wet", plotType="sus", site="Victoria")

ps_PRRI_sus=list(ps_16wetMalaSus,ps_16wetMaliSus,ps_17dryMalaSus,ps_17dryMaliSus,ps_17wetMalaSus,ps_17wetMaliSus,ps_18dryMalaSus,ps_18dryMaliSus,ps_18wetMalaSus,ps_18wetMaliSus)
ps_PRRI_res=list(ps_16wetMalaRes,ps_16wetMaliRes,ps_17dryMalaRes,ps_17dryMaliRes,ps_17wetMalaRes,ps_17wetMaliRes,ps_18dryMalaRes,ps_18dryMaliRes,ps_18wetMalaRes,ps_18wetMaliRes)

ps_IRRI_sus=list(ps_16wetPilaSus,ps_16wetVictSus,ps_17wetPilaSus,ps_17wetVictSus,ps_18dryPilaSus,ps_18dryVictSus,ps_18wetPilaSus,ps_18wetVictSus)
ps_IRRI_res=list(ps_16wetPilaRes,ps_16wetVictRes,ps_17wetPilaRes,ps_17wetVictRes,ps_18dryPilaRes,ps_18dryVictRes,ps_18wetPilaRes,ps_18wetVictRes)

get_metDat = function(year, season, site, plotSum, setMaxDAT){ #e.g. output = ps_16wetMaliRes
    #year = 2018; season=

    inst = name_institute(site)
    # filepath = paste0(
    #     "D:\\_Ryans-Folder\\OneDrive - Rothamsted Research\\_Workspace\\_PD4-IRRI-NFRR\\_Data\\Philippines\\WeatherData\\",
    #     inst, "\\"
    # )
    # metDat = read.csv(paste0(filepath, year, "_", season, "_", inst,"_Met.csv"))

    filepath = paste0(dataPath,"\\Philippines\\Field\\weather\\")
    metDat = read.csv(paste0(filepath,inst, "_", year, "_", season,".csv"))

    metDat$inst = inst
    metDat$site = site
    metDat$year = year
    metDat$season = season

    #Remove DAT Col of other sites.
    metDat = metDat[,-which(grepl("DAT_", names(metDat)) & !grepl(paste0("DAT_",site), names(metDat)))]
    metDat = metDat[,-which(grepl("DAS_", names(metDat)) & !grepl(paste0("DAS_",site), names(metDat)))]

    datCol = grep(paste0("DAT_",site), names(metDat))
    metDat[,datCol] = as.numeric(metDat[,datCol])
    #if(is.factor(metDat[,datCol])) metDat[,datCol] = suppressWarnings(as.numeric(levels(metDat[,datCol])[metDat[,datCol]]))
    metDat = metDat[!is.na(metDat[,datCol]),]
    if(setMaxDAT){ #this is for when I was letting DAT to exceed the experimental period in the met data, now set to NA, hence above code.
        metDat = metDat[
            metDat[,datCol]>=0 & metDat[,datCol]<=max(plotSum$DAT, na.rm=T)
            ,
            ]
    } else{
        metDat = metDat[
            metDat[,datCol]>=0 #& metDat[,datCol]<=max(plotSum$DAT, na.rm=T)
            ,
            ]
    }
    names(metDat)[datCol] = "DAT"

    if(is.factor(metDat$Rain)) metDat$Rain = as.numeric(metDat$Rain)

    return(metDat)
}

setMaxDAT = T
met_16wetMali = get_metDat(year=2016, season="Wet", plotSum=ps_16wetMaliSus, site="Maligaya", setMaxDAT=setMaxDAT) #year=2016; season="Wet"; plotSum=ps_16wetMaliSus; site="Maligaya"; setMaxDAT=setMaxDAT
met_16wetMala = get_metDat(year=2016, season="Wet", plotSum=ps_16wetMalaSus, site="Malayantoc", setMaxDAT=setMaxDAT) #year=2016; season="Wet"; plotSum=ps_16wetMalaSus; site="Malayantoc"; setMaxDAT=setMaxDAT
met_17dryMali = get_metDat(year=2017, season="Dry", plotSum=ps_17dryMaliSus, site="Maligaya", setMaxDAT=setMaxDAT)
met_17dryMala = get_metDat(year=2017, season="Dry", plotSum=ps_17dryMalaSus, site="Malayantoc", setMaxDAT=setMaxDAT)
met_17wetMali = get_metDat(year=2017, season="Wet", plotSum=ps_17wetMaliSus, site="Maligaya", setMaxDAT=setMaxDAT)
met_17wetMala = get_metDat(year=2017, season="Wet", plotSum=ps_17wetMalaSus, site="Malayantoc", setMaxDAT=setMaxDAT)
met_18dryMali = get_metDat(year=2018, season="Dry", plotSum=ps_18dryMaliSus, site="Maligaya", setMaxDAT=setMaxDAT)
met_18dryMala = get_metDat(year=2018, season="Dry", plotSum=ps_18dryMalaSus, site="Malayantoc", setMaxDAT=setMaxDAT)
met_18wetMali = get_metDat(year=2018, season="Wet", plotSum=ps_18wetMaliSus, site="Maligaya", setMaxDAT=setMaxDAT)
met_18wetMala = get_metDat(year=2018, season="Wet", plotSum=ps_18wetMalaSus, site="Malayantoc", setMaxDAT=setMaxDAT)

met_16wetPila = get_metDat(year=2016, season="Wet", plotSum=ps_16wetPilaSus, site="Pila", setMaxDAT=setMaxDAT)
met_16wetVict = get_metDat(year=2016, season="Wet", plotSum=ps_16wetVictSus, site="Victoria", setMaxDAT=setMaxDAT)
met_17wetPila = get_metDat(year=2017, season="Wet", plotSum=ps_17wetPilaSus, site="Pila", setMaxDAT=setMaxDAT)
met_17wetVict = get_metDat(year=2017, season="Wet", plotSum=ps_17wetVictSus, site="Victoria", setMaxDAT=setMaxDAT)
met_18dryPila = get_metDat(year=2018, season="Dry", plotSum=ps_18dryPilaSus, site="Pila", setMaxDAT=setMaxDAT)
met_18dryVict = get_metDat(year=2018, season="Dry", plotSum=ps_18dryVictSus, site="Victoria", setMaxDAT=setMaxDAT)
met_18wetPila = get_metDat(year=2018, season="Wet", plotSum=ps_18wetPilaSus, site="Pila", setMaxDAT=setMaxDAT)
met_18wetVict = get_metDat(year=2018, season="Wet", plotSum=ps_18wetVictSus, site="Victoria", setMaxDAT=setMaxDAT)

# metDat_PRRI = list(met_16wetMala,met_16wetMali,met_17dryMala,met_17dryMali,met_17wetMala,met_17wetMali,met_18dryMala,met_18dryMali,met_18wetMala,met_18wetMali)
# maxRain_PRRI = max(sapply(metDat_PRRI, function(x) max(x$Rain)))
# 
# metDat_IRRI = list(met_16wetPila,met_16wetVict,met_17wetPila,met_17wetVict,met_18dryPila,met_18dryVict,met_18wetPila,met_18wetVict)
# maxRain_IRRI = max(sapply(metDat_IRRI, function(x) max(x$Rain)))

get_maxMean = function(plotSum){
    means = aggregate(plotSum$numTillers~plotSum$DAT, FUN=mean)
    names(means) = c("DAT", "numTillers")
    return(max(means$numTillers))
}
auc = function(dat,xlab,ylab){
    dat[dat[,ylab]=="T",ylab]=0
    sum(
        diff(as.numeric(dat[,xlab])) * (head(as.numeric(dat[,ylab]),-1)+tail(as.numeric(dat[,ylab]),-1)) # x1 * (y1+y2)
    )/2
}

metData = list(
    met_16wetPila,
    met_16wetVict,
    met_16wetMala,
    met_16wetMali,
    met_17dryMala,
    met_17dryMali,
    met_17wetPila,
    met_17wetVict,
    met_17wetMala,
    met_17wetMali,
    met_18dryPila,
    met_18dryVict,
    met_18dryMala,
    met_18dryMali,
    met_18wetPila,
    met_18wetVict,
    met_18wetMala,
    met_18wetMali
)

siteNames = c(
    "met_16wetPila",
    "met_16wetVict",
    "met_16wetMala",
    "met_16wetMali",
    "met_17dryMala",
    "met_17dryMali",
    "met_17wetPila",
    "met_17wetVict",
    "met_17wetMala",
    "met_17wetMali",
    "met_18dryPila",
    "met_18dryVict",
    "met_18dryMala",
    "met_18dryMali",
    "met_18wetPila",
    "met_18wetVict",
    "met_18wetMala",
    "met_18wetMali"
)
#-----------------------------------------------------------------------------------TEMPERATURE----

tempMean = sapply(metData, function(x) mean(x$Tmean))
names(tempMean) = siteNames
tempMean

#------------------------------------------------------------------------------------------WIND----

windMean = sapply(metData, function(x) mean(x$Ws))
names(windMean) = siteNames
windMean

#------------------------------------------------------------------------------------------RAIN----

rainAUC = sapply(X=metData, FUN=auc, xlab="DAT", ylab="Rain")
names(rainAUC) = siteNames
rainAUC

#--------------------------------------------------------------------------------------------RH----

rhMean = sapply(metData, function(x) mean(x$RH))
names(rhMean) = siteNames
rhMean

#---------------------------------------------------------------Max Weekly-Mean Infection Count----

maxSusIRRI = sapply(X=ps_IRRI_sus, FUN=get_maxMean)
maxSusPRRI = sapply(X=ps_PRRI_sus, FUN=get_maxMean)

#Sus is always greater than Res so just need to report the Sus values.
maxResIRRI = sapply(X=ps_IRRI_res, FUN=get_maxMean)
maxSusIRRI>maxResIRRI
maxResPRRI = sapply(X=ps_PRRI_res, FUN=get_maxMean)
maxSusPRRI>maxResPRRI
