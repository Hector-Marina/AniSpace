#' Interpolate temporal-spatial information
#'
#' @description
#' `interpolate.AniSpace` interpolating individual temporal-spatial information.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param method A character variable indicating the selected interpolation method (Default: *method="makima"*): `prev` stads for interpolate using the previous position known, `linear` stands for a linear interpolation method, `spline` stands for a interpolation method based on drawing the individual trajectory based on a third degree polynom, `makima` stands for a interpolation method based on drawing the individual trajectory based on the modified akima algoritm and, `mixed` stands for a mixed interpolation method /under development/.
#' @param TRes A numeric variable in seconds indicating the output frequency of the position data (Default: *TRes=1*).
#' @param TCap A numeric variable indicating time threshold in seconds about of which interpolation will not be performed (Default: *TCap=120*).
#' @param tails A logical variable indicating whether the start and end of the day are filled with the first and last known positions (TRUE) or not (FALSE) (*Default: FALSE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords interpolation animal temporal spatial position information
#'
#' @return A interpolated animal position object
#'
#' @examples
#' df.inter=interpolate.AniSpace(df, method="makima", TRes=1)
#' df.inter
#'
#'
#' @export

interpolate.AniSpace=function(AniObj, method="makima", TRes=1, TCap=120, tails=FALSE, verbose=TRUE){

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (!method %in% c("prev", "linear", "spline", "makima")) {
    stop("Invalid `method`: must be one of 'prev', 'linear', 'spline', or 'makima'.")
  }

  if(!is.numeric(TRes))    stop("`freq` must be numeric")
  if(!is.numeric(TCap))    stop("`TCap` must be a even number")

  if(!is.logical(tails))   stop("`tails` must be logical")
  if(!is.logical(verbose)) stop("`verbose` must be logical")

  if(verbose) message("Interpolating position data...")
  l=lapply(seq_along(AniObj@Pos), function(ii) {
    int.obj=as.data.frame(AniObj@Pos[[ii]])

    # Duplicate the lines based in count
    int.obj$count=as.numeric(ceiling(c(int.obj$Time[2:nrow(int.obj)],int.obj$Time[nrow(int.obj)]+1)-int.obj$Time))
    int.obj=tidyr::uncount(int.obj,count,.remove = FALSE,.id="impid")

    # Filter by time cap
    int.obj$Lgroup=as.factor(int.obj$Time)
    levels(int.obj$Lgroup)=1:length(unique(int.obj$Time))
    int.obj$Lgroup=as.numeric(as.character(int.obj$Lgroup))
    W=unique(int.obj[int.obj$impid>TCap,"Lgroup"])
    int.obj=int.obj[!(int.obj$impid>1 & int.obj$Lgroup %in% W),]

    # Time adjust
    id=int.obj$impid==1
    ip=which(id)
    rp=which(!id)
    int.obj$Time[rp]=stats::approx(ip, int.obj$Time[id], xout=rp)$y

    # Position interpolation
    if (method=="linear"){
      int.obj$x[rp]=stats::approx(ip, int.obj$x[id], xout=rp)$y
      int.obj$y[rp]=stats::approx(ip, int.obj$y[id], xout=rp)$y
    } else if (method=="spline"){
      int.obj$x[rp]=stats::spline(ip, int.obj$x[id],method = "natural", xout=rp)$y
      int.obj$y[rp]=stats::spline(ip, int.obj$y[id],method = "natural", xout=rp)$y
    } else if (method=="makima"){
      int.obj$x[rp]=akima::aspline(ip, int.obj$x[id],ties = mean, method="improved", xout=rp)$y
      int.obj$y[rp]=akima::aspline(ip, int.obj$y[id],ties = mean, method="improved", xout=rp)$y
    }

    # Add tails
    if (tails){
      Tmin=0
      Tmax=AniObj@TLim[2]-AniObj@TLim[1]

      if(int.obj$Time[1]>Tmin){
        h=int.obj[1,,drop = FALSE][rep(1,(int.obj$Time[1])),]
        h$Time=c(Tmin:(int.obj$Time[1]-1))
        h$impid=0
        int.obj=rbind(h,int.obj)
      }
      if(int.obj$Time[nrow(int.obj)]<Tmax){
        t=int.obj[nrow(int.obj),,drop = FALSE][rep(1,(Tmax-int.obj$Time[nrow(int.obj)])),]
        t$Time=c((int.obj$Time[nrow(int.obj)]+1):Tmax)
        t$impid=0
        int.obj=rbind(int.obj,t)
      }
    }

    # Filter by frequency
    int.obj=int.obj[int.obj$Time %in% seq(Tmin, Tmax, by = TRes),]

    # Return resutls
    int.obj$Inter=!(int.obj$impid==1)
    int.obj=int.obj[!is.na(int.obj$y),]
    int.obj=int.obj[,c("Time","x","y","Inter")]

    if(verbose) sprog(ii,length(AniObj@NIDs))
    as.list(int.obj)
  })

  names(l)=names(AniObj@Pos)
  AniObj@Pos=l
  return(AniObj)
}
