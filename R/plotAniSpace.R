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
plotAniSpace=function(AniObj, NIDs=NULL, IDs=NULL, Area=NULL) {

  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if(is.null(NIDs) & is.null(IDs)){
    i=1
  }else if(!is.null(NIDs)){
    i=NIDs
  }else if(!is.null(IDs)){
    i=which(AniObj@IDs%in%IDs)
  }
  if (!any(i%in%AniObj@NIDs)) stop("Individuals not found in `AniObj`")

  if(is.null(Area)) {
    j=as.numeric(names(AniObj@Area))}
  if(is.character(Area)){
    j=which(Area%in%names(AniObj@Area))}

  if (any(j>length(AniObj@Area)) | length(j)<1) stop("Area not found in `AniObj`")

  # Create plot limited by the areas
  coords=do.call(rbind, lapply(AniObj@Area[j], function(a) a$coords[, c("x","y"), drop = FALSE]))
  xlim=range(coords[, "x"], na.rm = TRUE)
  ylim=range(coords[, "y"], na.rm = TRUE)

  # Image
  plot(NA, xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y", asp=1, xaxt='n')

  # Plot polygons
  for (jj in 1:length(j)) {
    polygon(c(AniObj@Area[[jj]]$coords[,"x"]),
            c(AniObj@Area[[jj]]$coords[,"y"]),
            col = AniObj@Area[[jj]]$color)}

  # Plot positions
  Rcols=rainbow(length(i))
  for (ii in 1:length(i)){
    points(x = AniObj@Pos[[i[ii]]]$x, y = AniObj@Pos[[i[ii]]]$y, col = Rcols[ii], cex=0.5)
  }

}
