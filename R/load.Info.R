#' Load individuals' information
#'
#' @description
#' `load.Info()` adds per-individual information (metadata) to an existing AniSpace object
#' by matching records in `InfObj` to `AniObj` using the identifier column `ID.col`.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param InfObj A data frame containing additional information for the individuals (one row per ID).
#' @param ID.col A character variable specifying the name of the column in `InfObj` that contains the animal IDs (*Default: "ID"*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords load transform animal information
#'
#' @return An AniSpace object
#'
#' @examples
#' # Create AniSpace object
#' df=read.Space(path="data/positions.csv", type="csv")
#'
#' # Reading the information from a file
#' af=read.csv("data/animals.csv", header=T)
#'
#' # Adding information to the AniSpace object
#' df=load.Info(AniObj=df, InfObj=af)
#' df
#'
#' @export

load.Info <- function(AniObj, InfObj, ID.col="ID", verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if(!is.data.frame(InfObj))         stop("`InfObj` must be a data.frame.")
  if(!is.character(ID.col))          stop("`ID.col` must be a non-empty character scalar.")
  # Check column names
  if(!any(ID.col  ==colnames(InfObj))) stop("Missing required column: ID.col")
  c=which(ID.col==colnames(InfObj))

  # Extract info from individuals presents in AniSpace obj
  l=match(df@IDs,as.character(InfObj$ID))
  ll=as.list(InfObj[l,-c, drop = FALSE])

  if (verbose) message("- A total of ",sum(is.na(l)), " individuals lack information (",round(sum(is.na(l))/length(l),2),"%)")

  # Add Information to the AniSpace object
  if(length(AniObj@Info)==0){
    AniObj@Info=ll
    if (verbose) message("- Information about ", paste(names(AniObj@Info), collapse = ", ")," has been added to the AniSpace object.")
  } else {
    a=setdiff(names(ll),names(AniObj@Info))
    if(length(a)>0){
      AniObj@Info=c(AniObj@Info,ll[a])
      if (verbose) message("- Information about: ", paste(names(ll[a]), collapse = ", "),"; has been added to the AniSpace object.")
    }else{
      if (verbose) message("- No information has been added to the AniSpace object.")
    }
  }

  return(AniObj)
}
