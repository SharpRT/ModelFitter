#' Convert sum of params to a single param ready for callSpecific()
#'
#' Convert sum of params to a single param ready for callSpecific()
#' @param params original set of params
#' @param aOrder list of params to be conjugated for each split
#' @param splitType the data to split the models by
#' @return a new param set that's a conjugation of the original params, in a format splittable by callSpecific()
#' @export
opt_conjugateParams=function(params, aOrder, splitType=NULL){
    names = names(params)
    mainPars = unique(unlist(aOrder))
    mainPar = unique(gsub("_.*","",mainPars))
    if(length(mainPar)>1) stop("Multiple Params to conjugate - not supported.")

    tempParams=c()
    if(!is.null(mainPars)){
        for(i in 1:length(aOrder)){#i=1
            tempParam = 0
            for(j in aOrder[[i]]){ #j=aOrder[[i]][1]
                tempParam = tempParam + params[j]
            }
            if(is.null(splitType)){
                newName = paste0(mainPar,"_",names(aOrder)[i])
            } else{
                newName = paste0(mainPar,"_",splitType,"_",names(aOrder)[i])
            }
            tempParams = c(tempParams, tempParam)
            names(tempParams)[length(tempParams)] = newName
        }
    }

    otherPars = names[!(names %in% mainPars)]
    for(i in otherPars){
        tempParams = c(tempParams, params[i])
    }
    # print(tempParams)
    return(tempParams)
}
