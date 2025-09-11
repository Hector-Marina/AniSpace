#' Transform individual spatio-temporal information into an AniSpace object
#'
#' @description
#' `load.Space()` transforms a data frame containing individual spatio-temporal information into the AniSpace format.
#'
#' @param PosObj A data frame containing the spatio-temporal information of the individuals.
#' @param Time.col A character variable specifying the name of the column containing the temporal information in epoch format in the file (*Default: "Time"*).
#' @param ID.col A character variable specifying the name of the column containing the animal ID information in the file (*Default: "ID"*).
#' @param x.col A character variable specifying the name of the column containing the spatial x-axis information in the file (*Default: "x"*).
#' @param y.col A character variable specifying the name of the column containing the spatial y-axis information in the file (*Default: "y"*).
#' @param TRes A numeric variable indicating the time resolution/frequency of the positions (*Default: 1 sec*).
#' @param Temp.sort A variable indicating whether the `load.Space` function should (TRUE) or should not (FALSE) sort the spatio-temporal information by `ID.col` and `Time.col` (*Default: TRUE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords transform animal spatial information
#'
#' @return An AniSpace object
#'
#' @examples
#' df=load.Space(data.frame)
#' df
#'
#' @export

load.Space <- function(PosObj, Time.col="Time", ID.col="ID", x.col="x", y.col="y", TRes=1, Temp.sort=TRUE, verbose=TRUE) {
  # Control parameters
  if(!is.data.frame(PosObj)) stop("`PosObj` must be a data.frame.")
  if(!all(is.character(Time.col), is.character(ID.col), is.character(x.col), is.character(y.col)))  stop("Column names (*.col) must be non-empty character scalars.")
  if(!is.numeric(TRes))      stop("`TRes` must be a positive, finite numeric scalar (seconds)")
  if(!is.logical(Temp.sort)) stop("`Temp.sort` must be a single logical value")

  # Check column names
  if(!any(Time.col==colnames(PosObj))) stop("Missing required column: Time.col")
  if(!is.numeric(PosObj[,Time.col]))   stop("`Time.col` must be a positive, finite numeric scalar (seconds)")
  if(!any(ID.col  ==colnames(PosObj))) stop("Missing required column: ID.col")
  if(!any(x.col   ==colnames(PosObj))) stop("Missing required column:  x.col")
  if(!is.numeric(PosObj[,x.col]))      stop("`x.col` must be a positive, finite numeric scalar (seconds)")
  if(!any(y.col   ==colnames(PosObj))) stop("Missing required column: y.col")
  if(!is.numeric(PosObj[,y.col]))      stop("`y.col` must be a positive, finite numeric scalar (seconds)")

  # sort data file
  if(Temp.sort) PosObj=PosObj[base::order(PosObj[,ID.col],as.numeric(PosObj[,Time.col])),]

  # Check data content
  if(!all(is.numeric(PosObj[,Time.col]),  is.numeric(PosObj[,x.col]), is.numeric(PosObj[,y.col]))) stop("Time and coordinates must be numeric")
  if(sum(!complete.cases(PosObj))>0){
    PosObj=PosObj[complete.cases(PosObj),]
    if(verbose) message(paste("Missing values were filtered from:", path))
  }

  # Handle Time correct epoch time (Seconds and/or miliseconds)
  s=ifelse(max(as.numeric(PosObj[,Time.col]))>1e11,1000,1)
  PosObj[,Time.col]=round(PosObj[,Time.col]/s,0)

  TLim=c(min(as.numeric(PosObj[,Time.col])),max(as.numeric(PosObj[,Time.col])))

  if(as.Date(Sys.Date())<as.Date(as.POSIXct(TLim[2], origin = "1970-01-01", tz="UTC"))){
    if(verbose) message("The dates in the dataset are older than the current system date (UTC).")
  }

  Pos=lapply(split(PosObj[, c(Time.col,x.col,y.col)], PosObj[,ID.col]), function(d) {
    d=d[!duplicated(d[, 1]), , drop = FALSE]  # keep first occurrence per timestamp
    m=list(Time = as.integer(d[, 1] - TLim[1]),
           x = as.numeric(d[, 2]),
           y = as.numeric(d[, 3]))
  })


  # Create the AniSpace object
  obj=new("AniSpace",
          NIDs = 1:length(unique(PosObj[,ID.col])),
          IDs  = as.character(unique(PosObj[,ID.col])),
          Info = list(),
          TLim = TLim,
          TRes = TRes,
          Pos  = Pos,
          Area = list())
  return(obj)
}
