#' [Title]
#'
#' [Description]
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @param year the year the data was collected
#' @param season the season the data was collected
#' @param type Either "I" or "N".
#' @param plotWeekSum data produced from \code{\link{calc_sumPlotWeek}}
#' @return plotWeekSum with compromised data removed.
#' @export
clean_plotWeekSum = function(site, year, season, type, plotWeekSum){
    if(site=="Malayantoc" & year==2016 & season=="Wet"){
        plotWeekSum = plotWeekSum[!(
                grepl("Sus", plotWeekSum$Plot_f)
                &plotWeekSum$Week==5
                &plotWeekSum$type=="I"
            )
            ,
        ]
    }

    return(plotWeekSum)
}
