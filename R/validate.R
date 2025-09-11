#' Validate AniSpace object's information
#'
#' @description
#' `validate()` Verifies and Consistency Checks for AniSpace Objects
#'
#' @param AniObj Performs a series of integrity and quality-control checks on an
#' `AniSpace` object or dataset, ensuring that positional, animal,
#' and area data meet expected structural and logical criteria.
#'
#' @examples
#' validate(df)
#'
#' @export
#'
validate=function(AniObj){
  VAL=TRUE

  # Validate NIDs IDs Info and Pos
  l=c(length(AniObj@NIDs),length(AniObj@IDs),length(AniObj@Pos))
  if(any(dist(l)>0)){
    VAL=FALSE
    message("AniObj validation: Individual identifiers are inconsistent with the position data.")
  }

  # Validate NIDs and Info
  if(length(AniObj@Info)>0){
    ll=length(AniObj@NIDs)
    l=sapply(seq_along(AniObj@Info), function(ii) {length(AniObj@Info[[ii]])})
    if(!all(ll==l)){
      VAL=FALSE
      message("AniObj validation: Individual identifiers are inconsistent with individuals' information.")
    }
  }

  # Validate TLim and TRes
  if(length(AniObj@TLim)!=2)        {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}
  if(!is.numeric(AniObj@TLim))      {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}
  if(AniObj@TLim[2]<AniObj@TLim[1]) {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}

  if(!is.numeric(AniObj@TRes) )     {message("AniObj validation: Time resolution information is incorrect.");VAL=FALSE}
  if(           AniObj@TRes<=0)     {message("AniObj validation: Time resolution information is incorrect.");VAL=FALSE}

  # Validate Pos
  l=sapply(seq_along(AniObj@Pos), function(ii) {
    length(AniObj@Pos[[ii]]$Time)== length(AniObj@Pos[[ii]]$x) &&
      length(AniObj@Pos[[ii]]$Time)== length(AniObj@Pos[[ii]]$y)})
  if(!all(l) )  {message("AniObj validation: Position information is incorrect (different lengths detected).");VAL=FALSE}
  l=sapply(seq_along(AniObj@Pos), function(ii) {
    is.numeric(AniObj@Pos[[ii]]$Time) && is.numeric(AniObj@Pos[[ii]]$x) && is.numeric(AniObj@Pos[[ii]]$y) })
  if(!all(l) )  {message("AniObj validation: Position information is incorrect (non-numeric information detected).");VAL=FALSE}

  # Area
  l=sapply(seq_along(AniObj@Area), function(ii) {length(AniObj@Area[[ii]])})
  if(any(dist(l)>0)){
    VAL=FALSE
    message("AniObj validation: Area information is inconsistent.")
  }
  l=sapply(seq_along(AniObj@Area), function(ii) {is.character(AniObj@Area[[ii]]$ID)})
  if(!all(l) )  {message("AniObj validation: Area ID information is incorrect (non-character information detected).");VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {is.matrix(AniObj@Area[[ii]]$coords)})
  if(!all(l) )  {message("AniObj validation: Area coords information is incorrect (non-matrix information detected).");VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {
    c=AniObj@Area[[ii]]$coords
    ifelse(!is.matrix(c) || ncol(c) != 2L  || nrow(c) < 4L || (nrow(c) >= 1L && !all(c[1, ] == c[nrow(c), ])),FALSE,TRUE)
  })
  if(!all(l) )  {message("AniObj validation: Area coords are incorrect or incorrect (non-matrix information detected).");VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {is.character(AniObj@Area[[ii]]$color)})
  if(!all(l) )  {message("AniObj validation: Area color information is incorrect (non-character information detected).");VAL=FALSE}

  return(VAL)
}
