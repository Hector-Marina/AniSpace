#' Estimate spatial interactions among individuals
#'
#' @description
#' `spatialint.AniSpace` estimates the pairwise spatial interactions between the individuals.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param method A variable indicating the type of proximity interactions to export (`time`, `nint` or `all`) (*Default: "time"*). `time` will export the duration spent by each pair of animals that are closer to each other than the specified `dist.thr` threshold. `nint` will record the number of instances where each pair of animals got within the specified `dist.thr` threshold of each other. `all` estimates both methods.
#' @param dist.thr A numeric variable indicating the distance threshold to be considered when estimating spatial interactions (*Default: 250 cm*).
#' @param time.thr A numeric variable indicating the time gap between spatial interactions to be considered a new interaction (*Default: 600 s*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @keywords animal spatial interactions information
#'
#' @return An adjacency matrix containing information about pairwise spatial interactions.
#'
#' @examples
#' # Estimate spatial interactions
#' df.AdjMat=spatialint.AniSpace(df)
#' AniPos.AdjMat[1:5,1:5]
#'
#' # Spatial interactions accounted as number of interactions
#' AniPos.AdjMat=spatialint.AniSpace(df, method="all")
#' AniPos.AdjMat[1:5,1:5]
#'
#' @export

spatialint.AniSpace=function(AniObj, method="time", dist.thr=250, time.thr=600, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (!method %in% c("time", "nint", "all")) {
    stop("Invalid `method`: must be one of 'time', 'nint', or 'all'.")
  }

  if(!is.numeric(dist.thr))  stop("`dist.thr` is not numeric")
  if(!is.numeric(time.thr))  stop("`time.thr` is not numeric")

  if(!is.logical(verbose)) stop("`verbose` must be logical")


  # Estimate distance, speed and turning angles
  l=lapply(seq_along(AniObj@Pos), function(ii) {
    data.frame(ID=AniObj@NIDs[ii],AniObj@Pos[[ii]])
  })
  pos.obj=do.call(rbind, l)

  # Input object information
  IDs=unique(AniObj@NIDs)
  Vtime=seq(min(pos.obj$Time),max(pos.obj$Time)+1,time.thr)

  # Output object
  AdjMat.obj=matrix(0,nrow = length(IDs), ncol = length(IDs))
  colnames(AdjMat.obj)=rownames(AdjMat.obj)=sort(IDs)

  # Split dataframe by Time
  if(verbose) message("Computing adjacency matrix for ",length(IDs)," individuals ...")
  pos.glist=split(pos.obj, pos.obj$Time)
  Ltime=as.numeric(names(pos.glist))

  for (i in 2:length(Vtime)){
    pos.list=pos.glist[(Ltime>=Vtime[(i-1)] & Ltime<Vtime[i])]

    # Estimate pairwise distance
    AdjMat.list=lapply(pos.list, function(l) {
      l=l[!duplicated(l$ID),]
      rownames(l)=l[,"ID"]
      AdjMat=as.matrix(dist(l[, c("x", "y")],diag = TRUE, upper = TRUE))
      AdjMat=ifelse(AdjMat < dist.thr, 1, 0)

      missIDs=setdiff(IDs,l[,"ID"])

      # Uniform matrices
      nr=matrix(0, nrow = length(missIDs), ncol = ncol(AdjMat))
      rownames(nr)=missIDs
      colnames(nr)=colnames(AdjMat)
      nc=matrix(0, nrow = nrow(AdjMat) + length(missIDs), ncol = length(missIDs))
      rownames(nc)=c(rownames(AdjMat), missIDs)
      colnames(nc)=missIDs

      # Add new rows and columns to the matrix
      mat   =rbind(AdjMat, nr)
      AdjMat=cbind(mat, nc)

      # Sort the matrix
      mat=AdjMat[order(rownames(AdjMat)),]
      AdjMat=mat[, order(colnames(mat))]

      return(AdjMat)
    })

    # Sum all matrices in the list
    AdjMat=Reduce("+", AdjMat.list)

    if(method=="time"){
      AdjMat.obj=Reduce("+", list(AdjMat.obj,AdjMat))
    }
    if(method=="nint"){
      if(i==2) {
        AdjMat.obj=Reduce("+", list(AdjMat.obj,AdjMat))
      }else{
        int.AdjMat=AdjMat
        int.AdjMat[,]=0
        int.AdjMat[AdjMat>old.AdjMat]=1
        AdjMat.obj=Reduce("+", list(AdjMat.obj,int.AdjMat))
      }
      old.AdjMat=AdjMat
    }
    if(verbose) sprog(i,length(Vtime))
  }
  return(AdjMat.obj)
}
