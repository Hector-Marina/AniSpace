#' Plot AniSpace position information
#'
#' @description
#' `plotAniSpace()` plot AniSpace position information
#'
#' @param AniObj An AniSpace object
#' @param NIDs Numeric. Vector containing the assigned index of the individuals (*Default: first*).
#' @param IDs  Character. Vector containing the identification of the individuals (*Default: first*).
#' @param Area Numeric. Information about the areas available for the individuals (Polygon format) (*Default: all*).
#'
#' @return Plot AniSpace position information
#'
#' @examples
#' validate(df)
#'
#' plot.AniSpace(df, IDs="2428706")
#'
#' plot.AniSpace(df, IDs="2428706", Area=names(df@Area)[c(1:7)])
#'
#' @export
#'
#'
plotAniSpace=function(AniObj, NIDs=NULL, IDs=NULL, Area=NULL, ...) {

  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if(is.null(NIDs) & is.null(IDs)){
    i=NULL
  }else if(!is.null(NIDs)){
    i=NIDs
  }else if(!is.null(IDs)){
    i=which(AniObj@IDs%in%IDs)
  }
  if (!is.null(i)){ if(!any(i%in%AniObj@NIDs)) stop("Individuals not found in `AniObj`")}

  if(is.null(Area)) {
    j=1:length(AniObj@Area)
  } else {
    if(is.character(Area)){
      l=sapply(seq_along(AniObj@Area), function(ii) {AniObj@Area[[ii]]$ID})
      if(!any(Area%in%l))          stop("`Area` not found in AniSpace object")
      j=which(l%in%Area)
    }else if (is.numeric(Area)){
      j=as.numeric(Area)
    } else { stop("`Area` must be class character or numeric") }
  }

  if (any(j>length(AniObj@Area)) | length(j)<1) stop("Area not found in `AniObj`")

  # Create plot limited by the areas
  coords=do.call(rbind, lapply(AniObj@Area[j], function(a) a$coords[, c("x","y"), drop = FALSE]))
  xlim=range(coords[, "x"], na.rm = TRUE)
  ylim=range(coords[, "y"], na.rm = TRUE)

  # Image
  plot(NA, xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y", asp=1, xaxt='n', ...)

  # Plot polygons
  for (jj in 1:length(j)) {
    polygon(c(AniObj@Area[[j[jj]]]$coords[,"x"]),
            c(AniObj@Area[[j[jj]]]$coords[,"y"]),
            col = AniObj@Area[[j[jj]]]$color)}

  # Plot positions
  if (!is.null(i)){
    Rcols=rainbow(length(i))
    for (ii in 1:length(i)){
      points(x = AniObj@Pos[[i[ii]]]$x, y = AniObj@Pos[[i[ii]]]$y, pch = 21, bg = Rcols[ii], col = Rcols[ii], cex=0.5)
    }
  }

}
