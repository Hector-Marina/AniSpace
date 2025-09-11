#' Convert the AniSpace object into a data frame
#'
#' @description
#' `Ani2list` export the position information from an AniSpace object to a data frame object.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords export position information
#'
#' @return A data frame that contains only the position information from the AniSpace object.
#'
#' @examples
#' df.tbl=Ani2tbl(AniObj=df)
#'
#' head(df.tbl)
#'
#' @export


Ani2tbl=function(AniObj, verbose=TRUE){
  # Control parameters
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  l=lapply(seq_along(AniObj@Pos), function(ii) {
    data.frame(NIDs=AniObj@NIDs[ii],IDs=AniObj@IDs[ii],AniObj@Pos[[ii]])
  })
  Pos_info=do.call(rbind, l)

  if (verbose) {
    message("Information on positions exported to the data frame:")
    message("- Total number of individuals: ", length(AniObj@NIDs))
    message("- Total number of records: ", nrow(Pos_info))
  }

  return(Pos_info)

}
