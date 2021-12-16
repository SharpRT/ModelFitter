#' @importFrom reshape2 melt
#' @importFrom utils read.csv
#' @importFrom rlang sym
#' @import dplyr
#' @import bindrcpp
NULL

#' Sum infected (I) and total tillers (N=H+I) from each plot-week
#'
#' Sums the number of infections and total number of tillers from each plot-week for a given site.
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @param year the year the data was collected
#' @param season the season the data was collected
#' @return dataframe with the date/DAT/Week# sampled, number of tillers infected and total number of tillers.
#' @export
calc_sumPlotWeek = function(data=NULL, country=NULL, experiment=NULL, site=NULL, year=NULL, season=NULL, sumVar="numTillers", filepath){

    if(is.null(data)){
        datMelt = load_all_experimentData(allDataTypes = data.frame(country=country, experiment=experiment, site=site, year=year, season=season), filepath=filepath)
    } else{
        datMelt = data
    }

    datMelt[,sumVar][datMelt[,sumVar]<0] = 0 #set any negative values to 0. - any deduction of -ve counts to N must be done prior to this.

    symVar=sym(sumVar)

    plotWeekSum = datMelt %>%
        group_by_(~Week, ~Line, ~Rep, ~type, ~site, ~year, ~season, ~country, ~DAT, ~WeekDate, ~Plot_f) %>%
        summarise(test = sum(!!symVar)) %>%
        as.data.frame()
    names(plotWeekSum)[match("test",names(plotWeekSum))] = sumVar

    country = unique(plotWeekSum$country)
    if(length(country)!=1) stop("Multiple countries in plotWeekSum!")

    #Add finalNSampleWeek
    plotWeekSum = add_finalNSampleWeek(data=plotWeekSum, allDataTypes=data.frame(site=site,year=year,season=season), country=country, experiment=experiment, filepath=filepath)
    plotWeekSum$isNRepeated = (plotWeekSum$Week > plotWeekSum$Final_N_Sample_Week) & plotWeekSum$type=="N"

    #Add plot type
    plotWeekSum$Plot_Type = c("sus", "res")[as.numeric(grepl("Res",plotWeekSum$Plot_f))+1]
    plotWeekSum = clean_plotWeekSum(site=site, year=year, season=season, plotWeekSum=plotWeekSum)

    return(plotWeekSum)
}
