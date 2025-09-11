#' Filtering temporal-spatial information
#'
#' @description
#' `filterAniSpace` filtering the individual temporal-spatial information.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param NIDs Numeric variable or vector containing the assigned index of the individuals (*Default: NULL*).
#' @param IDs  Character variable or vector containing the identification of the individuals (*Default: NULL*).
#' @param TimeWindow A numeric vector indicating the limits (minimum and maximum) of the time window to be maintained (*Default: NULL*).
#' @param Area A character variable or vector indicating the areas for which time/position data will be preserved (*Default: ALL*).
#' @param soft.boundaries A numeric variable indicating the distance threshold allowed outside the boundaries that will be brought back to the boundaries. The position reported after this threshold will be filtered (e.g. *soft.boundaries=2*; in the same unit of measurement that corresponds to the data (e.g. metres, centimetres, etc.) (*Default: 0*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords filter animal temporal spatial position information
#'
#' @return A filtered animal position object
#'
#' @examples
#' # Filter five individuals
#' df.ID.filt=filterAniSpace(df,NIDs=c(1:5))
#' df.ID.filt
#'
#' # Filter a specific time window
#' Tmin=as.numeric(as.POSIXct("2020-10-16 11:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#' Tmax=as.numeric(as.POSIXct("2020-10-16 11:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#' df.Time.filt=filterAniSpace(df,TimeWindow=c(Tmin,Tmax))
#' df.Time.filt
#'
#' # Filter positions from the resting area
#' df.Area.filt=filterAniSpace(df,Area=names(df@Area)[c(1:5)])
#' df.Area.filt
#'
#'
#' @export

filterAniSpace=function(AniObj, NIDs=NULL, IDs=NULL, TimeWindow=NULL, Area=NULL, soft.boundaries=0, verbose=TRUE) {
  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (is.null(NIDs) && is.null(IDs)) {
    NIDs=AniObj@NIDs
  } else if (is.null(NIDs) & !is.null(IDs)) {
    NIDs=which(AniObj@IDs %in% IDs)
  }
  if(!any(NIDs%in%AniObj@NIDs))stop("Individuals not found in `AniObj`")

  if(!is.null(TimeWindow)){
    if(!length(TimeWindow)==2)        stop("`TimeWindow` must contain the minimum and the maximum of the time window to be kept")
    if(!is.numeric(TimeWindow))       stop("`TimeWindow` must be numeric")
    if(TimeWindow[1]<AniObj@TLim[1])  stop("Min value of `TimeWindow` must be above the Time limit (see AniObj@TLim)")
    if(TimeWindow[2]>AniObj@TLim[2])  stop("Max value of `TimeWindow` must be below the Time limit (see AniObj@TLim)")
  }

  if(!is.null(Area)){
    if(!is.character(Area))                       stop("`Area` must be class character")
    l=sapply(seq_along(AniObj@Area), function(ii) {AniObj@Area[[ii]]$ID})
    if(!any(Area%in%l))          stop("`Area` not found in AniSpace object")
    NArea=which(l%in%Area)
  }

  if(!is.null(soft.boundaries)){
    if(!is.numeric(soft.boundaries))       stop("`soft.boundaries` must be class numeric")
  }
  if(!is.logical(verbose))     stop("`verbose` must be logical")

  #1)--- Filter individuals (NIDs)
  if (length(NIDs)<length(AniObj@NIDs)){
    if (verbose) message("- Filtering: ", length(NIDs)," individual(s) from the AniSpace object.")

    AniObj@NIDs=AniObj@NIDs[NIDs]
    AniObj@IDs =AniObj@IDs[NIDs]

    if(!is.null(AniObj@Info) && length(AniObj@Info) > 0){
      for (ii in c(1:length(AniObj@Info))){
        AniObj@Info[[ii]]=AniObj@Info[[ii]][NIDs]
      }
    }

    AniObj@Pos=AniObj@Pos[NIDs]
  }


  #2)--- Filter Time (TimeWindow)
  if(!is.null(TimeWindow)){
    if (verbose) message("- Filtering Time: ", TimeWindow[1]," (",as.character(as.POSIXct(TimeWindow[1], origin = "1970-01-01", tz="UTC")),") - ",
                                               TimeWindow[2]," (",as.character(as.POSIXct(TimeWindow[2], origin = "1970-01-01", tz="UTC")),") from the AniSpace object.")

    Tmin=TimeWindow[1]-AniObj@TLim[1]
    Tmax=TimeWindow[2]-AniObj@TLim[1]

    l=lapply(seq_along(AniObj@Pos), function(ii) {
      Time=AniObj@Pos[[ii]]$Time
      In=(Tmin<=Time & Time<=Tmax)
      return(list(Time=AniObj@Pos[[ii]]$Time[In],x=AniObj@Pos[[ii]]$x[In],y=AniObj@Pos[[ii]]$y[In]))
    })
    names(l)=names(AniObj@Pos)
    AniObj@Pos=l
  }


  #3)--- Filter Areas (NArea)
  if(!is.null(Area)){
    if (verbose) message("- Filtering: ", length(NArea)," area(s) from the AniSpace object.")

    for (ii in 1:length(AniObj@Pos)){
      vx=AniObj@Pos[[ii]]$x
      vy=AniObj@Pos[[ii]]$y

      In_mat=sapply(NArea, function(jj) {
          P=AniObj@Area[[jj]]$coords

          IN=ED=logical(length(vx))

          for (j in 1:(nrow(P) - 1L)) {
            x1=P[j, 1]; y1=P[j, 2]
            x2=P[j+1, 1]; y2=P[j+1, 2]
            dx=x2 - x1; dy=y2 - y1
            seglen2=dx*dx + dy*dy

            denom=y2 - y1
            crosses=((y1 > vy) != (y2 > vy)) & (denom != 0)
            xint=(dx * (vy - y1)) / denom + x1
            IN=xor(IN, crosses & (vx < xint))

            if (seglen2 > 0) {
              t=((vx - x1) * dx + (vy - y1) * dy) / seglen2
              t=pmin(1, pmax(0, t))            # clamp to segment
              cx=x1 + t * dx; cy=y1 + t * dy
              dist2=(vx - cx)^2 + (vy - cy)^2
              ED=ED | (dist2 <= soft.boundaries^2)
            } else {
              ED=ED | ((vx - x1)^2 + (vy - y1)^2 <= soft.boundaries^2)
            }
          }
          IN | ED
        }, simplify = "matrix")
      #In={rowSums(In_mat) > 0}
      In=if (length(In_mat) == 0L) rep(FALSE, length(vx)) else if (is.list(In_mat) && !is.matrix(In_mat)) Reduce(`|`, In_mat) else rowSums(as.matrix(In_mat) != 0) > 0L
      AniObj@Pos[[ii]][c("Time", "x", "y")]=lapply(AniObj@Pos[[ii]][c("Time", "x", "y")], `[`, In)
    }
  }


  #1)--- Remove NIDs that has no position data after the filtering
  l=sapply(seq_along(AniObj@Pos), function(ii) {length(AniObj@Pos[[ii]]$Time)})>0
  if(sum(l)==0) stop("No position information found after the filters were applied.")

  if(sum(l)<length(AniObj@NIDs)){
    if (verbose) message("- Filtering: ", sum(!l)," individual(s) had no position information after filtering.")

    AniObj@NIDs=AniObj@NIDs[l]
    AniObj@IDs =AniObj@IDs[l]

    if(!is.null(AniObj@Info) && length(AniObj@Info) > 0){
      for (ii in c(1:length(AniObj@Info))){
        AniObj@Info[[ii]]=AniObj@Info[[ii]][l]
      }
    }
    AniObj@Pos=AniObj@Pos[l]
  }

  # Rebase NIDs and Time variables
  AniObj=rebase(AniObj)

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("Invalid `AniObj` object.")

  return(AniObj)
}
