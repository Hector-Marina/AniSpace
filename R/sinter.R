#' Interpolate individual data
#'
#' @description
#' `sinter` Interpolate individual temporal-spatial information. This function is implemented in the `interpolate.AniPos` function.
#'
#' @param l A list object containing sorted numeric variables indicating the positions that need to be interpolated.
#' @param int.obj A data frame object containing sorted temporal-spatial information from an unique individual.
#' @param method A character variable indicating the interpolation method.
#'
#' @keywords cluster distance individual temporal spatial information
#'
#' @return A list of interpolated positions
#'
#' @examples
#' AniPos.id.inter=sinter(l,int.obj,method="linear")
#' head(AniPos.id.inter)
#'
#' @export

sinter <- function(l,int.obj,method) {
  if(!is.list(l))            stop("`l` must be a list object")
  if(!is.numeric(int.obj))   stop("`int.obj` must be numeric")
  if(!is.character(method))  stop("`method` must be character")

  if (nrow(cl.obj)>1){
    d=edist(x1=cl.obj$x,y1=cl.obj$y)

    hc=hclust(as.dist(d), method="complete")
    cl.obj$clust=cutree(hc, h=dist.thr)

    cl.obj$index=c(0,abs(cl.obj$clust[1:(nrow(cl.obj)-1)]-cl.obj$clust[2:nrow(cl.obj)]))
    cll=c(1,which(cl.obj$index>0),(nrow(cl.obj)+1))
    cl.obj$fc=NA;m=1
    for (k in 2:length(cll)) {cl.obj$fc[cll[k-1]:(cll[k]-1)]=m; m=m+1}

    ll2=match(unique(as.integer(cl.obj$fc)),as.integer(cl.obj$fc))
    cl.obj2=cl.obj[ll2,]

    cl.obj2[,"x"]=sapply(split(cl.obj[,"x"], as.integer(cl.obj$fc)), seq.fun, na.rm = TRUE)
    cl.obj2[,"y"]=sapply(split(cl.obj[,"y"], as.integer(cl.obj$fc)), seq.fun, na.rm = TRUE)
    if(!tpos) {
      cl.obj2[,"Time"]=round(sapply(split(cl.obj[,"Time"], as.integer(cl.obj$fc)), seq.fun, na.rm = TRUE),0)}

    return(cl.obj2)
  }
}

