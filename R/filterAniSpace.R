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
#' @param soft.boundaries A non-negative numeric value specifying the maximum distance outside an area's boundary at which positions are still retained (*Default: 0*).
#' @param bring.back A logical value indicating whether positions located outside the selected area should be moved inside the boundaries of that single area (*Default: FALSE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @return A filtered animal position object
#'
#' @examples
#' # Filter five individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=c(1:5))
#' df.ID.filt
#'
#' # Filter a specific time window
#' Tmin=as.numeric(as.POSIXct("2020-10-16 11:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#' Tmax=as.numeric(as.POSIXct("2020-10-16 11:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#' df.Time.filt=filterAniSpace(AniObj,TimeWindow=c(Tmin,Tmax))
#' df.Time.filt
#'
#' # Filter positions from the resting area
#' df.Area.filt=filterAniSpace(AniObj,Area=names(AniObj@Area)[c(1:5)])
#' df.Area.filt
#'
#'
#' @export

filterAniSpace=function(AniObj, NIDs=NULL, IDs=NULL, TimeWindow=NULL, Area=NULL, soft.boundaries=0, bring.back=FALSE ,verbose=TRUE) {
  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")


  if (length(AniObj@NIDs) == 0L || length(AniObj@IDs) == 0L) stop("No individuals were found in `AniObj`.")
  if (is.null(NIDs) && is.null(IDs)) {
    NIDs=AniObj@NIDs
  } else if (is.null(NIDs) & !is.null(IDs)) {
    NIDs=which(AniObj@IDs %in% IDs)
  }
  if(!any(NIDs%in%AniObj@NIDs)) stop("`Individual` not found in `AniObj`")

  if(!is.null(TimeWindow)){
    if(!length(TimeWindow)==2)        stop("`TimeWindow` must contain the minimum and the maximum of the time window to be kept")
    if(!is.numeric(TimeWindow))       stop("`TimeWindow` must be numeric")
    if(TimeWindow[1]<AniObj@TLim[1])  stop("Min value of `TimeWindow` must be above the Time limit (see AniObj@TLim)")
    if(TimeWindow[2]>AniObj@TLim[2])  stop("Max value of `TimeWindow` must be below the Time limit (see AniObj@TLim)")
  }

  if(!is.null(Area)){
    if (length(AniObj@Area) == 0L)    stop("No areas were found in `AniObj`.")
    if(!is.character(Area))           stop("`Area` must be class character")
    l=sapply(seq_along(AniObj@Area), function(ii) {AniObj@Area[[ii]]$ID})
    if(!any(Area%in%l))               stop("`Area` not found in AniSpace object")
    NArea=which(l%in%Area)
  }

  if(!is.null(soft.boundaries)){
    if (!is.numeric(soft.boundaries) || length(soft.boundaries) != 1L ||
        !is.finite(soft.boundaries)  || soft.boundaries < 0) stop("`soft.boundaries` must be class numeric")
  }
  if(!is.logical(bring.back))            stop("`bring.back` must be logical")
  if (bring.back && is.null(Area))       stop("An area must be selected when `bring.back = TRUE`.")
  if (bring.back && length(NArea) != 1L) stop("Only one area can be selected when `bring.back = TRUE`.")


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


    if (bring.back) {
      if (verbose) message("- Moving positions within `soft.boundaries` to the nearest boundary of area: ", Area,".")

      AniObj@Pos <- lapply(seq_along(AniObj@Pos), function(i) {
        p <- AniObj@Pos[[i]]
        vx <- p$x
        vy <- p$y
        P <- as.matrix(AniObj@Area[[NArea[1L]]]$coords)

        # Close polygon
        if (!all(P[1L, 1:2] == P[nrow(P), 1:2]))
          P <- rbind(P, P[1L, ])

        z <- lapply(seq_len(nrow(P) - 1L), function(j) {
          x1 <- P[j, 1];      y1 <- P[j, 2]
          dx <- P[j + 1L, 1] - x1
          dy <- P[j + 1L, 2] - y1
          d2 <- dx^2 + dy^2

          cross <- ((y1 > vy) != (y1 + dy > vy)) &
            (vx < x1 + dx * (vy - y1) / dy)

          t <- if (d2 > 0) pmin(1, pmax(0, ((vx - x1) * dx + (vy - y1) * dy) / d2)) else 0
          cx <- x1 + t * dx
          cy <- y1 + t * dy

          list(IN = cross, x = cx, y = cy, d2 = (vx - cx)^2 + (vy - cy)^2)
        })

        IN <- rowSums(do.call(cbind, lapply(z, `[[`, "IN")), na.rm = TRUE) %% 2L == 1L
        D  <- do.call(cbind, lapply(z, `[[`, "d2"))
        X  <- do.call(cbind, lapply(z, `[[`, "x"))
        Y  <- do.call(cbind, lapply(z, `[[`, "y"))

        D[!is.finite(D)] <- Inf
        jj <- max.col(-D, ties.method = "first")
        ij <- cbind(seq_along(vx), jj)
        move <- !IN & is.finite(D[ij]) & D[ij] <= soft.boundaries^2

        p$x[move] <- X[ij][move]
        p$y[move] <- Y[ij][move]

        p
      })
    }
  }

  #4)--- Remove NIDs that has no position data after the filtering
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

  #5)--- If exists filter or reset SI, UD and UDsim objects
  if(!is.null(TimeWindow) || !is.null(Area)){

    if(length(AniObj@SI)>0L){
      if(verbose)
        message("- Removing spatial-interaction information after filtering positions.")
      AniObj@SI=list()
    }

    if(length(AniObj@UD)>0L){
      if(verbose)
        message("- Removing utilisation-distribution information after filtering positions.")
      AniObj@UD=list()
    }

    if(length(AniObj@UDsim)>0L){
      if(verbose)
        message("- Removing utilisation-distribution similarity information after filtering positions.")
      AniObj@UDsim=list()
    }

  }else{
    if(length(AniObj@SI)>0L){
      if(length(AniObj@NIDs)<ncol(AniObj@SI$M)){
        AniObj@SI$M=AniObj@SI$M[
          AniObj@NIDs,
          AniObj@NIDs,
          drop=FALSE
        ]
      }
    }

    if(length(AniObj@UD)>0L){
      if(length(AniObj@UD$parameters$href)>1L &&
         length(AniObj@NIDs)<length(AniObj@UD$parameters$href)){
        AniObj@UD$parameters$href=
          AniObj@UD$parameters$href[AniObj@NIDs]
      }

      if(length(AniObj@UD$UD)>0L &&
         length(AniObj@NIDs)<length(AniObj@UD$UD)){
        AniObj@UD$UD=AniObj@UD$UD[AniObj@NIDs]
      }
    }

    if(length(AniObj@UDsim)>0L){
      if(length(AniObj@NIDs)<ncol(AniObj@UDsim$M)){
        AniObj@UDsim$M=AniObj@UDsim$M[
          AniObj@NIDs,
          AniObj@NIDs,
          drop=FALSE
        ]
      }
    }
  }

  # Rebase NIDs and Time variables
  AniObj=rebase(AniObj)

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("The filtered results produced an invalid `AniObj` object.")

  return(AniObj)
}
