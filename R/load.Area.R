#' Load areas' information
#'
#' @description
#' `load.Area()` load the information on the distribution of the barn areas.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param AreaObj A list containing additional information for the areas.
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords load area information
#'
#' @return An AniSpace object
#'
#' @examples
#' data(cows)
#' AniObj=load.Space(cows$positions)
#' AniObj=load.Info(AniObj,InfObj=cows$animals)
#'
#' # Convert rectangular area information to polygons
#' AreaObj=square2poly(cows$areas)
#'
#' # Add area information to the AniSpace object
#' AniObj=load.Area(AniObj,AreaObj)
#'
#' @export

load.Area <- function(AniObj, AreaObj, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")
  if(!is.list(AreaObj))              stop("`AreaObj` must be a list")


  # Verify the content of the list
  ## Step 1 — All areas contains valid info for ID, coods, and color
  ok1=vapply(AreaObj, function(z)
    !is.null(z$ID)    && is.character(z$ID)    && length(z$ID) == 1L && !is.na(z$ID) &&
      !is.null(z$coords) && is.matrix(z$coords) &&
      !is.null(z$color) && is.character(z$color) && length(z$color) == 1L && !is.na(z$color),
    logical(1))
  if (!all(ok1)) stop("`AreaObj` does not contain the right information. Please revise that there is no missing values.
                      `ID` and `color` colums are class characters and `coords` class matrix. Check areas: ",
                      paste(which(!ok1), collapse = ", "))

  ## Step 2 — coords validation (2 cols, ≥4 rows, closed polygon)
  for (i in seq_along(AreaObj)) {
    c=AreaObj[[i]]$coords
    if (!is.matrix(c) || ncol(c) != 2L)  stop("Coords must be a 2-col matrix. Check area:", i)
    if (nrow(c) < 4L)                    stop("Coords must have at least 4 rows (triangle + closure). Check area:", i)
    if (nrow(c) >= 1L && !all(c[1, ] == c[nrow(c), ])) stop("Coords must be closed (last row == first row). Check area:", i)
  }
  if (verbose) message("All ",length(AreaObj)," areas and their coordinates passed validation..")

  # Add Information to the AniSpace object
  if(length(AniObj@Area)==0){
    AniObj@Area=AreaObj
    n=sapply(AreaObj, `[[`, "ID") |> as.character()
    if (verbose) message("- Areas: ", paste(n, collapse = ", "),"; have been added to the AniSpace object.")
  } else {
    n1=sapply(AniObj@Area, `[[`, "ID") |> as.character()
    n2=sapply(AreaObj, `[[`, "ID") |> as.character()
    a=setdiff(n2,n1)

    if(length(a)>0){
      AniObj@Area=c(AniObj@Area,AreaObj[which(n2%in%a)])
      if (verbose) message("- Areas: ", paste(a, collapse = ", "),"; have been added to the AniSpace object.")
    }else{
      if (verbose) message("- No Area information has been added to the AniSpace object.")
    }
  }

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("Loading area information produced an invalid `AniObj` object.")

  return(AniObj)
}
