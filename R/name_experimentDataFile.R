
#' Generic filenaming tool
#'
#' Generates the name of the input file.
#' @param week week number.
#' @param plot plot number (between 1-6).
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @param type either "I" or "N".
#' @param year the year the data was collected
#' @param season the season the data was collected
#' @return file path and name of required file.
#' @export
name_experimentDataFile = function(country, experiment, week, plot, site, type, year, season){

    filename = name_basePath(country=country, experiment=experiment, site=site,year=year,season=season)
    if(country=="Philippines"){
        plotStrings = unlist(lapply(seq(1:3),function(x) paste0(c("Res", "Sus"),x)))
        # plotStrings = 1:6
        filename = paste0(
            filename,
            "Week_",week,"\\",
            site,"_Week",week,"_Plot",plotStrings[plot],"_",type,".csv"
        )
    } else if(country=="Thailand"){
        if(experiment=="Field"){
            plotStrings = unlist(lapply(seq(1:7),function(x) paste0("_Line",x,"_",c("Rep1","Rep2","Rep3"))))
        } else if(experiment=="Box"){
            plotStrings = paste0("_Pathotype", c(1:4))
        }
        filename = paste0(
            filename,
            experiment,"Dat_", site, "_" , type, "_Week", week, plotStrings[plot], ".csv"
        )
    }

    return(filename)
}


#' Generic filenaming tool
#'
#' Generates the name of the input file.
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @param year the year the data was collected
#' @param season the season the data was collected
#' @return file path and name of required file.
#' @export
name_basePath = function(country, experiment, site, year, season){
    basepath = paste0(country, "\\", experiment, "\\", year,"\\")
    if(country=="Philippines"){
        basepath = paste0(basepath,
            season,"\\",
            name_institute(site),"_",site,"\\"
        )
    }
    return(basepath)
}
