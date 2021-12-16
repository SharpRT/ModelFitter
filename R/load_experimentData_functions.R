#' @importFrom plyr join
#' @import dplyr
NULL

#' Load both infected and total tiller count data from the given experimental sites, year and season.
#'
#' Load both infected and total tiller count data from the given experimental sites, year and season.
#' @param allDataTypes data frame giving a series of year/season/site combinations to load
#' @param plots the plots to load
#' @param levels levels to be provided to plot factor
#' @param labels labels to be provided to plot factor
#' @param clean perform post-load modifications to the data
#' @return dataframe containing all spatially resolved data
#' @export
load_all_experimentData = function(allDataTypes,plots=NULL,levels=NULL,labels=NULL,clean=T, filepath){

    allData = c()
    types=c("I","N") #Why do I load both types? - To update N with dead I values.

    for(i in 1:nrow(allDataTypes)){#i=1
        country = allDataTypes$country[i]
        experiment = allDataTypes$experiment[i]
        year = allDataTypes$year[i]
        season = allDataTypes$season[i]
        site = allDataTypes$site[i]

        allTypes=c()

        for(type in types){#type="I"; type="N"
            allTypes = tryCatch(
                {
                    rbind(
                        allTypes,
                        load_experimentData(
                            country=country, experiment=experiment, site=site, type=type, year=year, season=season, clean=clean,
                            plots=plots, levels=levels, labels=labels, datFilepath = filepath
                        )
                    )
                },
                error=function(cond) {
                    message("\nError:")
                    message(cond)
                    # Choose a return value in case of error
                    return(NA)
                },
                warning=function(cond) {
                    message("\nWarning:")
                    message(cond)
                    # Choose a return value in case of warning
                    return(NULL)
                }
            )
        }

        allData = tryCatch(
            {
                rbind(allData, allTypes)
            },
            error=function(cond) {
                message("\nError:")
                message(cond)
                # Choose a return value in case of error
                return(NA)
            },
            warning=function(cond) {
                message("\nWarning:")
                message(cond)
                # Choose a return value in case of warning
                return(NULL)
            }
        )
    }

    if(clean){
        allData = update_dead_experimentData(allData)
    }

    return(allData)
}

#' Load infected tiller data from a given experimental site
#'
#' Load infected tiller data from a given experimental site
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @param type either "I" or "N".
#' @param year the year the data was collected
#' @param season the season the data was collected
#' @param clean perform post-load modifications to the data
#' @param plots the plots to load
#' @param levels levels to be provided to plot factor
#' @param labels labels to be provided to plot factor
#' @param datFilepath location of site data
#' @return dataframe containing spatially resolved experiment data from a given site
#' @export
load_experimentData = function(
    country, experiment, site, type, year, season, clean,
    plots = NULL, levels = NULL, labels = NULL,
    datFilepath #datFilepath=filepath
){
    if(country=="Philippines"){
        numLines = 2
        numReps = 3
        lines = rep(x=1:numLines, t=numReps)
        reps = unlist(lapply(c(1:numReps),function(x) rep(x,t=numLines)))
        if(is.null(labels)) labels = c("Res1","Sus1","Res2","Sus2","Res3","Sus3")
    } else if(country=="Thailand"){
        if(experiment=="Field"){
            numLines = 7
            numReps = 3
            lines = unlist(lapply(c(1:numLines),function(x) rep(x,t=numReps)))
            reps = rep(x=1:numReps, t=numLines)
            if(is.null(labels)) labels = unlist(lapply(seq(1:numLines),function(x) paste0("L",x,c("R1","R2","R3"))))
        } else if(experiment=="Box"){
            numLines = 4 #pathotypes
            numReps = 1 #1 plot for each pathotype
            lines = 1:4
            reps = rep(1,4)
            if(is.null(labels)) labels = c("P1","P2","P3","P4")
        }
    }
    if(is.null(plots)) plots = 1:(numLines*numReps)
    if(is.null(levels)) levels = 1:(numLines*numReps)

    sampleDates = load_sampleDatesFile(country, experiment, filepath = datFilepath)
    filteredDates = sampleDates[as.character(sampleDates$Site)==site & sampleDates$Year==year & as.character(sampleDates$Season)==season,]

    siteDates = sampleDates$WeekDate[as.character(sampleDates$Site)==site & sampleDates$Year==year & as.character(sampleDates$Season)==season]
    siteDATs = sampleDates$DAT[as.character(sampleDates$Site)==site & sampleDates$Year==year & as.character(sampleDates$Season)==season]

    numWeeks = length(siteDates)
    numPlots = length(plots)

    pltList = vector("list",numPlots)
    for(j in 1:numPlots){#j=1
        wkList = vector("list", numWeeks)
        for(i in 1:numWeeks){#i=1; i=i+1
            mat = as.matrix(read.csv(
                paste0(datFilepath,name_experimentDataFile(country=country, experiment=experiment, week=i, plot=plots[j], site=site, type=type, year=year, season=season)),
                header=F
            ))
            colnames(mat)=NULL
            wkList[[i]] = mat
        }
        pltList[[j]] = melt(wkList)
        if(country=="Thailand"){
            names(pltList[[j]]) = c("Row","Column", "numTillers", "Week")
        } else if(country=="Philippines"){
            names(pltList[[j]]) = c("Column","Row", "numTillers", "Week")
        }
    }

    megaMelt = melt(pltList, id=c("Column","Row","numTillers","Week"))

    megaMelt = join(megaMelt, filteredDates, by="Week")

    names(megaMelt)[5] = "Plot"
    megaMelt$Week_f = factor(
        megaMelt$Week,
        levels=c(1:length(unique(megaMelt$Week))),
        labels=paste0(format(siteDates,format="%d-%b"), " (", siteDATs, " DAT)")
    )
    megaMelt$Plot_f = factor(
        megaMelt$Plot,
        levels=levels,
        labels=labels
    )

    megaMelt$type = factor(rep(type, dim(megaMelt)[1]), levels=c("I","N"))
    megaMelt$country = country
    megaMelt$experiment = experiment

    if(clean){
        megaMelt = clean_experimentData(megaMelt)
    }

    if(country=="Thailand" && experiment=="Box"){
        pathotypeDF = data.frame(Plot=plots,Pathotype=lines)
        megaMelt = join(megaMelt,pathotypeDF,by="Plot")

        megaMelt$Rep = ifelse(megaMelt$Column<=5,1,2)
        lineDF = read.csv(paste0(datFilepath,"\\",country,"\\",experiment,"\\",year,"\\BoxLineCoordinates.csv"))
        megaMelt = join(megaMelt,lineDF,by=c("Site", "Pathotype", "Row", "Rep"))
        megaMelt = megaMelt[c(
            "Column","Row","numTillers","Week","Pathotype","Line","Rep","Plot","Week_f","Plot_f",
            "SowingDate","TransplantDate","Diff","WeekDate","DAS","DAT",
            "country", "experiment","Site","type","Year","Season"
        )]
    } else if(country=="Thailand" && experiment=="Field"){
        plotDF = data.frame(Plot=plots,Line=lines,Rep=reps)
        megaMelt = join(megaMelt,plotDF,by="Plot")

        fieldPosDF = read.csv(paste0(datFilepath,"\\",country,"\\",experiment,"\\FieldPlotPos.csv"))
        megaMelt = join(megaMelt, fieldPosDF, by=c("Site", "Line", "Rep"))


        megaMelt$FieldHill_X_BS2 = (max(megaMelt$Column)+2)*(megaMelt$x-1)+1+(megaMelt$Column-1)
        megaMelt$FieldHill_Y_BS2 = (max(megaMelt$Row)+2)*(megaMelt$y-1)+max(megaMelt$Row)-(megaMelt$Row-1)

        megaMelt = megaMelt[c(
            "Column","Row","numTillers","Week","Line","Rep","Plot","Week_f","Plot_f",
            "SowingDate","TransplantDate","Diff","WeekDate","DAS","DAT",
            "country", "experiment","Site","type","Year","Season","x","y","FieldHill_X_BS2","FieldHill_Y_BS2"
        )]
    } else{
        plotDF = data.frame(Plot=plots,Line=lines,Rep=reps)
        megaMelt = join(megaMelt,plotDF,by="Plot")
        megaMelt = megaMelt[c(
            "Column","Row","numTillers","Week","Line","Rep","Plot","Week_f","Plot_f",
            "SowingDate","TransplantDate","Diff","WeekDate","DAS","DAT",
            "country", "experiment","Site","type","Year","Season"
        )]
    }
    names(megaMelt)[names(megaMelt)=="Site"] = "site"
    names(megaMelt)[names(megaMelt)=="Year"] = "year"
    names(megaMelt)[names(megaMelt)=="Season"] = "season"

    return(megaMelt)
}

#' Clean spatially resolved infected tiller count experiment data
#'
#' Clean spatially resolved infected tiller count experiment data
#' @param allData dataframe containing all spatially resolved experiment data
#' @return modified dataframe containing all spatially resolved experiment data
#' @export
clean_experimentData = function(allData){
    #Remove 2016_Wet_Malayantoc_Week5_Sus data as compromised.
    allData$numTillers[
        grepl("Sus",allData$Plot_f)
        &allData$site=="Malayantoc"
        &allData$Week==5
        &allData$year==2016
        &allData$season=="Wet"
        &allData$type=="I"
    ] = NA

    return(allData)
}

#' Mark dead hills
#'
#' Marks dead hills by setting values to -1.
#' @param allData dataframe containing all spatially resolved experiment data
#' @return modified dataframe containing all spatially resolved experiment data
#' @export
update_dead_experimentData = function(allData){
    I = allData[allData$type=="I",]
    N = allData[allData$type=="N",]

    I$numTillers[I$numTillers==0 & N$numTillers==0 & !is.na(I$numTillers) & !is.na(N$numTillers)] = -1
    N$numTillers[I$numTillers==-1 & !is.na(I$numTillers) & !is.na(N$numTillers)] = -1

    allData = rbind(I,N)

    return(allData)
}
