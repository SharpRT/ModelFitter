
library(NFRRPhilippines)
library(stats4)
library(ggplot2)

#debug
# library(plyr)
# library(dplyr)
# library(reshape2)

rm(list=ls())

dataPath = "D:/_temp/extdata/"

#LOAD DATA-----------------------------------------------------------------

#splitting data into two plots, one for Malayanctoc and Maligaya (PRRI) and one for Pila and Victoria (IRRI)
isPRRI = F

#Specify which dataset to work with.]
country = "Philippines"
experiment = "Field"

#Specify which measure of time to use.
tColStr = "DAT" #DAT recommended.

if(isPRRI){
    allDataTypes = NULL
    allDataTypes = rbind(allDataTypes, data.frame(year=2016, season="Wet", site="Malayantoc")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2016, season="Wet", site="Maligaya")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Dry", site="Malayantoc")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Dry", site="Maligaya")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Wet", site="Malayantoc")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Wet", site="Maligaya")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Dry", site="Malayantoc")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Dry", site="Maligaya")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Wet", site="Malayantoc")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Wet", site="Maligaya")) #
    fileout="PRRIdata"
} else{
    allDataTypes = NULL
    allDataTypes = rbind(allDataTypes, data.frame(year=2016, season="Wet", site="Pila")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2016, season="Wet", site="Victoria")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Wet", site="Pila")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2017, season="Wet", site="Victoria")) #
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Dry", site="Pila")) #MISSING
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Dry", site="Victoria")) #MISSING
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Wet", site="Pila")) #MISSING
    allDataTypes = rbind(allDataTypes, data.frame(year=2018, season="Wet", site="Victoria")) #
    fileout="IRRIdata"
}

data = NULL
model = NULL

for(row in 1:nrow(allDataTypes)){ #row=1

    type = allDataTypes[row,]
    year = type$year
    season = type$season
    site = type$site

    # year = 2016 #2016, 2017
    # season = "Wet" #2016->Wet, 2017->Dry
    # site = "Pila"#c("Chiang-Saen", "Mae-Sai", "Phan") # "Chiangrai", "kps"

    key = paste0(year, "_", season, "_", site)

    plotSum = calc_sumPlotWeek(country=country, experiment=experiment, site=site, year=year, season=season, sumVar="numTillers", filepath=dataPath) #Load the data
    #If you get an error message re. a corrupt file here, it may be because R has not been restarted since installing the package.

    plotSum=plotSum[!is.na(plotSum$numTillers),] #Remove NA values.
    plotSum = plotSum[plotSum$type=="I",] #Analyse just the infected count data.

    print(paste0("Loading ", key))
    #Remove certain data from model fit (outliers).
    if(any(key==c(#------------------------------rmLastWeek
        "2016_Wet_Pila",
        "2016_Wet_Victoria",
        "2016_Wet_Malayantoc",
        "2017_Wet_Pila",
        "2017_Wet_Victoria",
        "2017_Wet_Maligaya",
        "2018_Dry_Maligaya",
        "2018_Wet_Victoria"
    ))){
        #plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        tmpKeep = plotSum[plotSum$Week!=max(plotSum$Week),]
        tmpDisc = plotSum[plotSum$Week==max(plotSum$Week),]
    } else if(any(key==c(#--------------------------rmWeek4
        "2017_Dry_Maligaya"
    ))){
        #plotSum = plotSum[plotSum$Week!=4,]
        tmpKeep = plotSum[plotSum$Week!=4,]
        tmpDisc = plotSum[plotSum$Week==4,]
    } else if(any(key==c(#---------------------rmLast2Weeks
        "2018_Wet_Malayantoc"
    ))){
        # plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        # plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        tmpKeep = plotSum[plotSum$Week<=(max(plotSum$Week)-2),]
        tmpDisc = plotSum[plotSum$Week>(max(plotSum$Week)-2),]
    } else if(any(key==c(#---------------------rmLast3Weeks
        "2018_Wet_Maligaya"
    ))){
        # plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        # plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        # plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
        tmpKeep = plotSum[plotSum$Week<=(max(plotSum$Week)-3),]
        tmpDisc = plotSum[plotSum$Week>(max(plotSum$Week)-3),]
    } else if(any(key==c(#--------------------rmFirst3Weeks
        "2018_Dry_Malayantoc"
    ))){
        # plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
        # plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
        # plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
        tmpKeep = plotSum[plotSum$Week>=(min(plotSum$Week)+3),]
        tmpDisc = plotSum[plotSum$Week<(min(plotSum$Week)+3),]
    } else{
        tmpKeep = plotSum
        tmpDisc = NULL
    }

    tmpKeep$keep = "Modelled Data"
    print("dim(tmpKeep)")
    print(dim(tmpKeep))
    data = rbind(data, tmpKeep)
    if(!is.null(tmpDisc)){
        tmpDisc$keep = "Discarded Data"
        print("dim(tmpDisc)")
        print(dim(tmpDisc))
        data = rbind(data, tmpDisc)
    }

    m = as.data.frame(modelOut_philippinesStandard(year=year, season=season, site=site, filepath=dataPath))

    # why do the plots look strange? - it is a valid behaviour of the model, this code is fine
    m = rbind(c(0,0,0),m)

    # #do I want to discard this part of the model? #could just ensure plotted axis uses data$DAT range? -
    # if(m$DAT[1]<min(tmpKeep$DAT)){
    #     m = m[-1,]
    # }

    # m

    #wide to long format
    mRes = data.frame(DAT=m$DAT,numTillers=m$I_R)
    mSus = data.frame(DAT=m$DAT,numTillers=m$I_S)
    mRes$Plot_Type = "Resistant"
    mSus$Plot_Type = "Susceptible"
    tmpModel = rbind(mRes,mSus)

    tmpModel$site = site
    tmpModel$year = year
    tmpModel$season = season

    model = rbind(model, tmpModel)
}

# unique(data$DAT)
# dim(data)
# head(data)
# unique(data$season)
# str(data)

data$keep = factor(x = data$keep, levels = c("Modelled Data", "Discarded Data"))
model$keep = "Modelled Data"
model$keep = factor(x = model$keep, levels = c("Modelled Data", "Discarded Data"))
model$lineName = "Model"
data$Plot_Type[data$Plot_Type=="res"] = "Resistant"
data$Plot_Type[data$Plot_Type=="sus"] = "Susceptible"

#Now plot it... begin with data, then integrate model.

minDAT = min(data$DAT)
maxDAT = max(data$DAT)

p=ggplot()+
    geom_point(aes(x=DAT,y=numTillers, fill=keep), data = data, pch = 21)+
    scale_fill_manual(values = c(rgb(0.75,0.75,0.75), "black"))+
    geom_line(aes(x=DAT, y=numTillers, linetype=lineName), data = model, colour="black")+
    facet_grid(year+season ~ site+Plot_Type)+
    ylab("Number of infected tillers\n")+
    xlab("\nDays after transplant")+
    coord_cartesian(xlim=c(minDAT, maxDAT))+
    theme_light()+
    theme(
        legend.position = "bottom",
        strip.background =element_rect(fill=rgb(0.5,0.5,0.5)),
        legend.title = element_blank()
    )
p

outpath="D:/_temp/"

w_unit=2
h_unit=3
units = "cm"
base = 19/3
width = (base*3/2)*w_unit #19/3*3/3*2 = 19
height = (base)*h_unit #19/3*3=19
res=1000

ggsave(
    filename = paste0(outpath,fileout,".tif"),
    plot = p,
    device = 'tiff', width = width, height = height, units = units, dpi = res, compression = "lzw"
)
