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
#' # Create AniSpace object
#' df=read.Space(path="data/positions.csv", type="csv")
#'
#' # Add manually the information
#' cf=list(
#' list(ID="0",
#'      coords=matrix(c(0,   0, 3340,  0, 3340,2902, 0,2902, 0,0), ncol=2, byrow=TRUE, dimnames=list(NULL,c("x","y"))),
#'      color="beige"),
#' list(ID="1",
#'      coords=matrix(c(0,2902, 630,2902, 630,8738, 0,8738, 0,2902), ncol=2, byrow=TRUE, dimnames=list(NULL,c("x","y"))),
#'      color="green"
#' ))
#'
#' # Reading the information from a file
#' bf=read.csv("data/barnplan.csv", header=T)
#' cf=square2poly(bf)
#'
#' # Adding information to the AniSpace object
#' df=load.Info(AniObj=df, AreaObj=cf)
#' df
#'
#' @export

load.Area <- function(AniObj, AreaObj, ID.col="ID", verbose=TRUE) {

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

  return(AniObj)
}
