#' Smooth individual movement trajectories
#'
#' @description
#' `smooth` smooths individual movement trajectories by applying a running median to the `x` and `y` coordinates stored in an `AniSpace` object.
#'
#' @param AniObj An object of class `AniSpace` containing the individuals' spatio-temporal position information.
#' @param smooth.wind A positive odd integer specifying the size of the running-median window. (Default: *smooth.wind=3*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @return The modified `AniSpace` object with smoothed individual trajectories.
#'
#' @examples
#' # Smooth individual trajectories using a 3-position running median
#' df.smooth=smooth(AniObj,smooth.wind=3,verbose=FALSE)
#' df.smooth
#'
#' # Compare original and smoothed coordinates for the first individual
#' head(data.frame(
#'   x=AniObj@Pos[[1]]$x,
#'   x.smooth=df.smooth@Pos[[1]]$x,
#'   y=AniObj@Pos[[1]]$y,
#'   y.smooth=df.smooth@Pos[[1]]$y
#' ))
#'
#' @export

smooth=function(AniObj, smooth.wind=3, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace"))      stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))                  stop("Invalid `AniObj` object.")

  if (!is.numeric(smooth.wind) || length(smooth.wind) != 1L ||
      !is.finite(smooth.wind)  || smooth.wind <= 0 ||
      smooth.wind %% 1 != 0    || smooth.wind %% 2 == 0)    stop("`smooth.wind` must be a single positive odd integer.")

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

  # Check irregular_time
  irregular_time=any(vapply( AniObj@Pos,function(p) {dt <- diff(p$Time); length(dt) > 1L && any(dt != dt[1L])}, logical(1)))
  if(verbose && irregular_time) warning("The smoothing function assumes a constant time interval between position data.")

  # Check position information
  p_short=vapply(AniObj@Pos, function(p) length(p$Time) < smooth.wind, logical(1))
  if (verbose && any(p_short)) {
    message("- ", sum(p_short), " individual(s) had fewer than ",  smooth.wind, " positions and were not smoothed.")
  }

  AniObj@Pos=lapply(seq_along(AniObj@Pos), function(i) {
    p=AniObj@Pos[[i]]
    if (length(p$Time) < smooth.wind) return(p)

    smooth.x=stats::runmed(p$x, k=smooth.wind, endrule="keep")
    smooth.y=stats::runmed(p$y, k=smooth.wind, endrule="keep")
    p$x=smooth.x
    p$y=smooth.y

    return(p)
  })

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("Smoothing produced an invalid `AniObj` object.")

  return(AniObj)
}
