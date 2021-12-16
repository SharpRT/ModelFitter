#' Get name of institute
#'
#' Get the name of the institute handling a given site.
#' @param site site name (one of "Pila","Victoria","Malayantoc","Maligaya").
#' @return name of institute corresponding to \code{site}
#' @export
name_institute = function(site){
    if(site=="Pila" || site=="Victoria"){
        return("IRRI")
    } else if(site=="Malayantoc" || site=="Maligaya"){
        return("PRRI")
    } else{
        return(NULL)
    }
}
