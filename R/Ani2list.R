#' Convert the AniSpace object into a list
#'
#' @description
#' `Ani2list` export information from an AniSpace object to a list object.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords export animal position area information
#'
#' @return  A list that contains the information from the AniSpace object.
#'
#' @examples
#' df.list=Ani2list(AniObj=df)
#'
#' names(df.list)
#'
#' head(df.list$Pos_info)
#'
#'
#' @export

Ani2list=function(AniObj, verbose=TRUE){
  # Control parameters
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  # Extract animal information to data frame
  if(!is.null(AniObj@Info) && length(AniObj@Info) > 0){
    IDs_info=data.frame(NIDs=AniObj@NIDs,IDs=AniObj@IDs,AniObj@Info)
    if (verbose) {
      message("Information on animals exported to the list:")
      message("- Total number of traits: ", ncol(IDs_info)-2)
    }
  }else{IDs_info=data.frame()}

  # Extract position information to data frame
  l=lapply(seq_along(AniObj@Pos), function(ii) {
    data.frame(NIDs=AniObj@NIDs[ii],IDs=AniObj@IDs[ii],AniObj@Pos[[ii]])
  })
  Pos_info=do.call(rbind, l)
  if (verbose) {
    message("Information on positions exported to the list:")
    message("- Total number of individuals: ", length(AniObj@NIDs))
    message("- Total number of records: ", nrow(Pos_info))
  }

  # Extract area information to data frame
  if(!is.null(AniObj@Area) && length(AniObj@Area) > 0){
    l=lapply(seq_along(AniObj@Area), function(ii) {
      data.frame(ID=AniObj@Area[[ii]]$ID,
                 minX=min(AniObj@Area[[ii]]$coords[,"x"]), maxX=max(AniObj@Area[[ii]]$coords[,"x"]),
                 minY=min(AniObj@Area[[ii]]$coords[,"y"]), maxY=max(AniObj@Area[[ii]]$coords[,"y"]),
                 color=AniObj@Area[[ii]]$color)})
    Area_info=do.call(rbind, l)
    if (verbose) {
      message("Information on areas exported to the list:")
      message("- Total number of areas: ", length(AniObj@NIDs))
    }
  }else{Area_info=data.frame()}


  return(list(IDs_info=IDs_info,Pos_info=Pos_info,Area_info=Area_info))

}
