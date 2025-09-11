#' Filtering and smoothing spikes in the temporal-spatial information
#'
#' @description
#' `spikes.AniSpace` filtering or smoothing spikes from the individual temporal-spatial information using a speed threshold.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param method A variable indicating the selected approach (`filter` or `smooth`) (Default: *method="smooth"*). `filter` remove position that surpass the speed threshold limit (`max.speed`). `smooth` smooth the spikes by applying a running median sliding window of size equal to `smooth.wind`.
#' @param smooth.wind A numeric variable indicating the size of the sliding window applied on `smooth.spikes`. The number must me odd (Default: *smooth.wind=5*).
#' @param max.speed A numeric variable indicating the maximum speed allow (distance/temporal units, e.g. m/s). Example: *max.speed=50* (Default: *max.speed=NULL*).
#' @param perc.speed A numeric variable E(0,1) indicating the top percentile of the global speed distribution that should be used as `max.speed` to filter the spikes in the position data (Default: *perc.speed=0.99*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @keywords filter smooth spikes noise animal temporal spatial position information
#'
#' @return A spikes filtered and/or smoothed animal position object
#'
#' @examples
#' # Filter spikes
#' df.spikes.filt =spikes.AniSpace(df, method="filter")
#'
#' # Smooth spikes
#' df.smooth.filt =spikes.AniSpace(df, method="smooth")
#'
#' @export

spikes.AniSpace <- function(AniObj, method="smooth", smooth.wind=5,
                                    max.speed=NULL, perc.speed=0.99,
                                    verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace"))      stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))                  stop("Invalid `AniObj` object.")

  if (!method %in% c("filter", "smooth")) {
    stop("Invalid `method`: must be one of 'filter' or 'smooth'.")
  }
  if(!is.numeric(smooth.wind))        stop("`smooth.wind` is not numeric")

  if(!is.numeric(perc.speed))             stop("`perc.speed` is not numeric")
  if(!(0 < perc.speed & perc.speed < 1))  stop("`perc.speed` is not a number between 0 and 1")
  if(all(!is.null(max.speed) & !is.numeric(max.speed)))       stop("`max.speed` must be numeric")

  if(!is.logical(verbose))  stop("`verbose` is not logical")

  # Estimate max.speed if null
  if (is.null(max.speed)) {
    max.speed=stats::quantile(do.call(c,l), probs = perc.speed, na.rm = TRUE)
    if (verbose)  message("The maximum speed threshold (`max.speed`) was estimated from the ", perc.speed * 100,"th percentile of the speed distribution: ", max.speed, " dist/s.")
  }

  # Speed over max.speed threshold
  l=lapply(seq_along(AniObj@Pos), function(i) {
    p=AniObj@Pos[[i]]

    if (length(p$Time) < 2L) {return(NA)}

    dt=diff(as.numeric(p$Time))
    dx=diff(as.numeric(p$x))
    dy=diff(as.numeric(p$y))
    dist =sqrt(dx^2 + dy^2)
    s = dist / dt

    ok=is.finite(s) & is.finite(dist) & dt > 0
    s=s[ok]
    s=which(s > max.speed) + 1L
    return(s)
  })

  # Remove spikes
  if(method=="filter"){
    if (verbose)  message("Filtering spikes...")
    AniObj@Pos=lapply(seq_along(AniObj@Pos), function(i) {
        p=AniObj@Pos[[i]]
        if (length(l[[i]]) > 0L)  p=lapply(p, function(x) x[-l[[1]]])
        return(p)
    })
  }

  # Smooth spikes
  if(method=="smooth"){

    if (verbose) {
      message("Smoothing spikes...")
      if(any(diff(AniObj@Pos[[i]]$Time)>1)) warning("The smoothing function assumes a constant time interval between position data.")

      if (smooth.wind %% 2 == 0) {
        smooth.wind = smooth.wind + 1
        warning("The `smooth.wind` variable needs to be an odd number. Converted to: ",smooth.wind)
      }
    }

    AniObj@Pos=lapply(seq_along(AniObj@Pos), function(i) {
      p=AniObj@Pos[[i]]
      if (length(l[[i]]) == 0L) return(p)
      if(length(p$Time)<smooth.wind) warning("The number of total position for ID: ",AniObj@IDs[i]," is lower than `smooth.wind` value.")

      smooth.x=stats::runmed(p$x, k=smooth.wind, endrule="keep")
      smooth.y=stats::runmed(p$y, k=smooth.wind, endrule="keep")
      p$x[l[[i]]]=smooth.x[l[[i]]]
      p$y[l[[i]]]=smooth.y[l[[i]]]

      return(p)
    })
  }

  # Print number of filtered/smoothed positions
  if (verbose) message("An average of ", round(mean(sapply(l,length)),2), " positions per animal were ", if (method=="filter") "filtered." else "smoothed.")

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("Invalid `AniObj` object.")

  return(AniObj)
}
