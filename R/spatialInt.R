#' Estimate pairwise spatial interactions among individuals
#'
#' @description
#' `spatialInt` estimates pairwise proximity interactions among individuals based on their spatial coordinates and a specified distance threshold.
#'
#' @param AniObj An object of class `AniSpace` containing the individuals' spatio-temporal position information.
#' @param method A variable indicating the type of proximity interactions to export (`time`, `nint` or `all`) (*Default: "all"*).
#' `time` will export the duration per pair of animals in closer proximity than the specified `dist.thr` threshold.
#' `nint` will export the number of instances where each pair of animals got within the specified `dist.thr` threshold of each other.
#' `all` estimates both methods.
#' @param dist.thr A positive numeric value specifying the maximum distance between two individuals for them to be considered interacting. It must use the same spatial units as the coordinates stored in `AniObj.` (*Default: 250* (cm)).
#' @param time.thr A positive numeric value specifying the time interval used to distinguish separate proximity interactions. It must use the same time units as the Time values stored in `AniObj` (*Default: 600* (s)).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @return The modified `AniSpace` object, with the estimated pairwise spatial-interaction adjacency matrix (SI) stored in `AniObj`.
#'
#' @examples
#' # Estimate spatial interactions
#' df.SI=spatialInt(AniObj)
#' df.SI@SI$M[1:5,1:5]
#'
#' @export

spatialInt=function(AniObj, method="all", dist.thr=250, time.thr=600, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (!method %in% c("time", "nint", "all")) {
    stop("Invalid `method`: must be one of 'time', 'nint', or 'all'.")
  }

  if (!is.numeric(dist.thr) || length(dist.thr) != 1L ||
    !is.finite(dist.thr)    || dist.thr <= 0)    stop("`dist.thr` must be a single positive numeric value.")
  if (!is.numeric(time.thr) || length(time.thr) != 1L ||
      !is.finite(time.thr)    || time.thr <= 0)  stop("`time.thr` must be a single positive numeric value.")

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

  # Number of observations per individual
  n_ind=length(AniObj@NIDs)
  n_obs=vapply(AniObj@Pos, function(p) length(p$Time), integer(1))
  ind=rep.int(seq_len(n_ind), n_obs)

  time=unlist(lapply(AniObj@Pos, function(p) as.numeric(p$Time)),use.names = FALSE)
  x   =unlist(lapply(AniObj@Pos, function(p) as.numeric(p$x)),   use.names = FALSE)
  y   =unlist(lapply(AniObj@Pos, function(p) as.numeric(p$y)),   use.names = FALSE)

  # Sort time
  ord=order(time, ind)
  ind =ind[ord]
  time=time[ord]
  x   =x[ord]
  y   =y[ord]

  # Locate consecutive groups of equal times
  time_runs    =base::rle(time)
  group_end    =cumsum(time_runs$lengths)
  group_start  =group_end - time_runs$lengths + 1L
  unique_times =time_runs$values

  matrix_names =as.character(AniObj@NIDs)

  # Selecting method
  need_time =method %in% c("time", "all")
  need_nint =method %in% c("nint", "all")

  if (need_time) time_mat=matrix(0, nrow = n_ind, ncol = n_ind, dimnames = list(matrix_names, matrix_names))
  if (need_nint) {
    nint_mat    =matrix(0, nrow = n_ind, ncol = n_ind, dimnames = list(matrix_names, matrix_names))
    last_contact=matrix(-Inf, nrow = n_ind, ncol = n_ind)
  }
  dist_thr2=dist.thr^2

  if (verbose) message("Estimating pairwise spatial interactions among ", n_ind, " individuals across ",
                        length(unique_times), " observed time points using method='",method,"'.")

  for (tt in seq_along(unique_times)) {

    # Extract group
    rows =group_start[tt]:group_end[tt]
    group_ind =ind[rows]
    group_x   =x[rows]
    group_y   =y[rows]
    if (length(group_ind) < 2L) next

    # Squared pairwise distances
    dx =outer(group_x, group_x, "-")
    dy =outer(group_y, group_y, "-")

    # Evaluate proximity
    proximity=dx^2 + dy^2 <= dist_thr2
    diag(proximity)=FALSE
    if (!any(proximity)) next

    # Duration spent in proximity
    if (need_time) {
      current=time_mat[group_ind,group_ind,drop = FALSE]
      current=current + proximity * AniObj@TRes
      time_mat[group_ind, group_ind]=current
    }

    # Number of distinct interactions
    if (need_nint) {
      previous        =last_contact[group_ind,group_ind,drop = FALSE]
      new_interaction = proximity & (unique_times[tt] - previous > time.thr)

      current=nint_mat[group_ind,group_ind,drop = FALSE]
      current=current + new_interaction
      nint_mat[group_ind, group_ind]=current

      previous[proximity]=unique_times[tt]
      last_contact[group_ind, group_ind]=previous
    }
  }

  # Combine results if neccesary
  M=matrix(0,nrow = n_ind, ncol = n_ind, dimnames = list(matrix_names, matrix_names))

  if (need_time){
    diag(time_mat)=0
    M[upper.tri(M)]=time_mat[upper.tri(time_mat)]

  }
  if (need_nint){
    diag(nint_mat)=0L
    M[lower.tri(M)]=nint_mat[lower.tri(nint_mat)]
  }

  if(verbose & sum(M)==0) warning("No interaction detected between the ",n_ind," individuals analysed.")

  # Add matrix to AniObj
  AniObj@SI=list(method = method, dist.thr = dist.thr, time.thr = time.thr, M=M)

  # Validate AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("The spatial-interaction results produced an invalid `AniObj` object.")

  return(AniObj)
}
