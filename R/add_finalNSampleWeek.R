#' [Title]
#'
#' [Description]
#' @param data data produced from \code{\link{calc_sumPlotWeek}}
#' @param allDataTypes data frame giving a series of year/season/site combinations to load
#' @param filepath location of folder containing sample dates data files
#' @export
add_finalNSampleWeek = function(
    data, allDataTypes, filepath,
    country, experiment
){
    oriFNSW = read.csv(
        paste0(filepath, "\\", country, "\\", experiment, "\\", "Final_N_Week.csv")
    )

    oriDat = data
    finDat = NULL
    for(i in 1:nrow(allDataTypes)){ #i=1 i=2
        year = allDataTypes$year[i]
        season = allDataTypes$season[i]
        site = allDataTypes$site[i]

        finalNSampleWeek = oriFNSW[
            as.character(oriFNSW$Season)==season
            & oriFNSW$Year==year
            & as.character(oriFNSW$Site)==site
            ,
        ]

        data = oriDat[
            as.character(oriDat$season)==season
            & oriDat$year==year
            & as.character(oriDat$site)==site
            ,
        ]
        data$Final_N_Sample_Week = finalNSampleWeek$Final_N_Sample_Week[match(data$Plot_f, finalNSampleWeek$Plot)]

        finDat = rbind(finDat,data)
    }


    return(finDat)
}
