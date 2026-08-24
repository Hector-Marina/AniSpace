#' Filtering and smoothing spikes in the temporal-spatial information
#'
#' @description
#' `spikes` filtering or smoothing spikes from the individual temporal-spatial information using a speed threshold.
#'
#' @param AniObj An `AniSpace` object containing the spatio-temporal information of the individuals.
#' @param method A variable indicating the selected approach (`filter` or `smooth`) (Default: *method="smooth"*). `filter` remove position that surpass the speed threshold limit (`max.speed`). `smooth` smooth the spikes by applying a running median sliding window of size equal to `smooth.wind`.
#' @param smooth.wind A numeric variable indicating the size of the sliding window applied on `smooth.spikes`. The number must me odd (Default: *smooth.wind=5*).
#' @param max.speed A numeric variable indicating the maximum speed allow (distance/temporal units, e.g. m/s). Example: *max.speed=50* (Default: *max.speed=NULL*).
#' @param perc.speed A numeric variable E(0,1) indicating the top percentile of the global speed distribution that should be used as `max.speed` to filter the spikes in the position data (Default: *perc.speed=0.99*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @return A spikes filtered and/or smoothed `AniSpace` object
#'
#' @examples
#' # Filter positions identified as spikes
#' df.spikes=spikes(AniObj,method="filter",perc.speed=0.99,verbose=FALSE)
#' df.spikes
#'
#' # Compare the number of positions before and after filtering
#' c(
#'   original=sum(vapply(AniObj@Pos,function(p) length(p$Time),numeric(1))),
#'   filtered=sum(vapply(df.spikes@Pos,function(p) length(p$Time),numeric(1)))
#' )
#'
#' # Smooth positions identified as spikes
#' df.smooth=spikes(AniObj,method="smooth",smooth.wind=5,
#'                  perc.speed=0.99,verbose=FALSE)
#' df.smooth
#'
#' @export
spikes=function(AniObj,method="smooth",smooth.wind=5,
                max.speed=NULL,perc.speed=0.99,
                verbose=TRUE){

  # Control parameters
  if(!inherits(AniObj,"AniSpace"))
    stop("`AniObj` must be class 'AniSpace'.")
  if(!validate(AniObj))
    stop("Invalid `AniObj` object.")

  if(!method %in% c("filter","smooth"))
    stop("Invalid `method`: must be one of 'filter' or 'smooth'.")

  if(!is.numeric(smooth.wind) || length(smooth.wind)!=1L ||
     !is.finite(smooth.wind) || smooth.wind<=0 ||
     smooth.wind%%1!=0 || smooth.wind%%2==0)
    stop("`smooth.wind` must be a single positive odd integer.")

  if(!is.null(max.speed) &&
     (!is.numeric(max.speed) || length(max.speed)!=1L ||
      !is.finite(max.speed) || max.speed<=0))
    stop("`max.speed` must be a single positive numeric value.")

  if(!is.numeric(perc.speed) || length(perc.speed)!=1L ||
     !is.finite(perc.speed) || perc.speed<=0 || perc.speed>=1)
    stop("`perc.speed` must be a numeric value between 0 and 1.")

  if(!is.logical(verbose) || length(verbose)!=1L || is.na(verbose))
    stop("`verbose` must be either TRUE or FALSE.")

  # Calculate speeds
  speed=lapply(seq_along(AniObj@Pos),function(i){
    p=AniObj@Pos[[i]]

    if(length(p$Time)<2L)
      return(numeric())

    dt=diff(as.numeric(p$Time))
    dx=diff(as.numeric(p$x))
    dy=diff(as.numeric(p$y))

    s=sqrt(dx^2+dy^2)/dt
    s[!is.finite(s) | dt<=0]=NA_real_

    return(s)
  })

  # Estimate max.speed if NULL
  if(is.null(max.speed)){
    all.speed=unlist(speed,use.names=FALSE)
    all.speed=all.speed[is.finite(all.speed)]

    if(length(all.speed)==0L)
      stop("No valid speed values were available to estimate `max.speed`.")

    max.speed=as.numeric(
      stats::quantile(all.speed,probs=perc.speed,na.rm=TRUE)
    )

    if(verbose)
      message(
        "The maximum speed threshold (`max.speed`) was estimated from the ",
        perc.speed*100,
        "th percentile of the speed distribution: ",
        round(max.speed,3)," dist/s."
      )
  }

  # Identify positions following speeds above max.speed
  l=lapply(speed,function(s){which(is.finite(s) & s>max.speed)+1L})

  # Remove spikes
  if(method=="filter"){
    if(verbose) message("Filtering spikes...")

    AniObj@Pos=lapply(seq_along(AniObj@Pos),function(i){
      p=AniObj@Pos[[i]]

      if(length(l[[i]])>0L)
        p=lapply(p,function(x) x[-l[[i]]])

      return(p)
    })
  }

  # Smooth spikes
  if(method=="smooth"){

    if(verbose){
      message("Smoothing spikes...")

      irregular_time=any(vapply(AniObj@Pos, function(p){
          dt=diff(p$Time)
          length(dt)>1L && any(dt!=dt[1L])
        }, logical(1)))

      if(irregular_time)
        warning(
          "The smoothing function assumes a constant time interval between position data."
        )
    }

    AniObj@Pos=lapply(seq_along(AniObj@Pos),function(i){
      p=AniObj@Pos[[i]]

      if(length(l[[i]])==0L) return(p)

      if(length(p$Time)<smooth.wind){
        warning("The number of positions for ID: ",  AniObj@IDs[i],
          " is lower than `smooth.wind`.")
        return(p)
      }

      smooth.x=stats::runmed(p$x,k=smooth.wind,endrule="keep")
      smooth.y=stats::runmed(p$y,k=smooth.wind,endrule="keep")

      p$x[l[[i]]]=smooth.x[l[[i]]]
      p$y[l[[i]]]=smooth.y[l[[i]]]

      return(p)
    })
  }

  # Print number of filtered/smoothed positions
  if(verbose)
    message(
      "An average of ",
      round(mean(vapply(l,length,numeric(1))),2),
      " positions per animal were ",
      if(method=="filter") "filtered." else "smoothed."
    )

  # Validate AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("The spike handling produced an invalid `AniObj` object.")

  return(AniObj)
}
