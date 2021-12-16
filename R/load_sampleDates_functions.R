#' @importFrom utils read.csv
NULL

#' Load sampling dates of given site
#'
#' Loads dates that a given site was sampled.
#' @param filepath location of folder containing sample dates data files
#' @return read SampleDates file data.
#' @export
load_sampleDatesFile = function(country, experiment, filepath){
    filename = paste0(filepath,country,"\\",experiment,"\\SampleDates.csv")
    if(country=="Philippines"){
        dates = read.csv(
            filename,
            colClasses=c(
                Site=NA,
                SowingDate="Date",
                TransplantDate="Date",
                Diff=NA,
                Week=NA,
                WeekDate="Date",
                Year=NA,
                Season=NA,
                DAS=NA,
                DAT=NA,
                Plot1="Date",
                Plot2="Date",
                Plot3="Date",
                Plot4="Date",
                Plot5="Date",
                Plot6="Date",
                PlotDateRange=NA
            )
        )
    } else if(country=="Thailand"){
        dates = read.csv(
            filename,
            colClasses=c(
                Site=NA,
                SowingDate="Date",
                TransplantDate="Date",
                Diff=NA,
                Week=NA,
                WeekDate="Date",
                Year=NA,
                Season=NA,
                DAS=NA,
                DAT=NA
            )
        )
    }
    return(dates)
}
