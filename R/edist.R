#' Estimate euclidean distance
#'
#' @description
#' `edist` estimate euclidean distance between temporal-position information
#'
#' @param x1 set number one of temporal-position information in the X axis
#' @param y1 set number one of temporal-position information in the Y axis
#' @param x2 set number two of temporal-position information in the X axis
#' @param y2 set number two of temporal-position information in the Y axis
#'
#' @keywords euclidean distance temporal spatial information
#'
#' @return a vector with the Euclidean distance information corresponding to the input values
#'
#' @examples
#' euclidean.dist=edist(x1,y1,x2,y2)
#' head(euclidean.dist)
#'
#' @export

edist <- function(x1=NULL,y1=NULL,x2=NULL,y2=NULL) {

  # Control parameters
  if(!is.numeric(x1))         stop("`x1` needs to be numberic")
  if(!is.numeric(y1))         stop("`y1` needs to be numberic")
  if(is.null(x2)) x2=x1
  if(is.null(y2)) y2=y1
  if(!is.numeric(`x2`))         stop("`x2` needs to be numberic")
  if(!is.numeric(`y2`))         stop("`y2` needs to be numberic")

  if (length(x1) >1 & length(x2) >1){
    if(!(length(x1)==length(y1))) stop("`x1` and `y1` need to be the same length")
    if(!(length(x2)==length(y2))) stop("`x2` and `y2` need to be the same length")
    if(length(x1)>10000)          stop("The length of the temporal-positional information must to be less than 10000")
      d=matrix(0,nrow = length(x1),ncol = length(x2))
      for (i in 1:length(x1)){
        d[i,]=sqrt((x1[i] - x2)^2 + (y1[i] - y2)^2)
      }
  } else {
    d=sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }
  return(d)
}
