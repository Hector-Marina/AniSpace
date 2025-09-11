#' Read individual temporal-spatial information
#'
#' @description
#' `read.Space()` load the individual temporal-spatial information into the R environment.
#'
#' @param path A string containing the location and full name of the file to load.
#' @param type A variable indicating the type of file to be loaded: `csv` (comma (,) separated), `json` or `json.gz` (*Default: csv*).
#' @param header A logical variable specifying if the animal information file contains headers  (*Default: TRUE*).
#' @param col.names A character string containing the new names of the columns in the file; they are assigned automatically after the file has been read.
#' @param Time.col A character variable specifying the name of the column containing the temporal information in epoch format in the file (*Default: "Time"*).
#' @param ID.col A character variable specifying the name of the column containing the animal ID information in the file (*Default: "ID"*).
#' @param x.col A character variable specifying the name of the column containing the spatial x-axis information in the file (*Default: "x"*).
#' @param y.col A character variable specifying the name of the column containing the spatial y-axis information in the file (*Default: "y"*).
#' @param TRes A numeric variable indicating the time resolution/frequency of the positions (*Default: 1 sec*).
#' @param Temp.sort A variable indicating whether the `load.Space` function should (TRUE) or should not (FALSE) sort the spatio-temporal information by `ID.col` and `Time.col` (*Default: TRUE*).
#' @param na.strings A string to interpret as missing values, passed to read.csv() (*Default: "NA"*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords read load animal spatial information csv json
#'
#' @return An AniSpace object
#'
#' @importFrom data.table fread
#' @importFrom utils read.csv
#' @importFrom jsonlite stream_in
#'
#' @examples
#' df=read.Space(path="data/positions.csv", type="csv")
#' df
#'
#' @export

read.Space=function(path, type="csv",
                        header=TRUE, col.names=NULL,
                        Time.col="Time", ID.col="ID", x.col="x", y.col="y", TRes=1,
                        Temp.sort=TRUE, na.strings="NA", verbose=TRUE) {
  # Control parameters
  if(!file.exists(path))     stop(paste0("File: ",path," not found"))
  if(!any(type==c("csv","json","json.gz"))) stop("Value of `type` unrecognised")
  if(!is.logical(header))    stop("`header` value must be logical")
  if(!is.null(col.names) & !is.character(col.names)) stop("`col.names` must be character")
  if(!all(is.character(Time.col), is.character(ID.col), is.character(x.col), is.character(y.col)))  stop("Column names (*.col) must be characters")
  if(!is.numeric(TRes))      stop("`TRes` value must be numeric")
  if(!is.logical(Temp.sort)) stop("`Temp.sort` value must be logical")

  # Read file
  if(type=="csv") {
    obj=if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::fread(
        path, header = header, na.strings = na.strings,
        data.table = FALSE, showProgress = FALSE,
        check.names = FALSE, integer64 = "numeric"
      )
    } else {
      utils::read.csv(
        path, header = header, na.strings = na.strings,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  } else  if(type=="json") {
    obj=suppressMessages(suppressWarnings(jsonlite::stream_in(path, verbose = F)))
    obj=cbind(obj[,c(Time.col,ID.col)],obj[,"coordinates"][,c("x","y")])
    colnames(obj)=c(Time.col,ID.col,"coordinates.x","coordinates.y")

  } else  if(type=="json.gz") {
    obj=suppressMessages(suppressWarnings(jsonlite::stream_in(gzfile(path), verbose = F)))
    obj=cbind(obj[,c(Time.col,ID.col)],obj[,"coordinates"][,c("x","y")])
    colnames(obj)=c(Time.col,ID.col,"coordinates.x","coordinates.y")
  }

  # Rename columns
  if(!is.null(col.names)) {
    if (length(col.names) != ncol(obj)) stop("`col.names` length must match number of columns in the file.")
    colnames(obj)=col.names
  }

  # Check/Rename column names
  if(header){
    if(!any(Time.col==colnames(obj))) stop("Column name `Time.col` not found")
    if(!any(ID.col  ==colnames(obj))) stop("Column name `ID.col` not found")
    if(!any(x.col   ==colnames(obj))) stop("Column name `x.col` not found")
    if(!any(y.col   ==colnames(obj))) stop("Column name `y.col` not found")
  }
  # sort data file
  if(Temp.sort) obj=obj[base::order(obj[,ID.col],as.numeric(obj[,Time.col])),]

  # Check data content
  if(!all(is.numeric(obj[,Time.col]),  is.numeric(obj[,x.col]), is.numeric(obj[,y.col]))) stop("`Time` and coordinates (`x` and `y`) must be numeric")
  if(sum(!complete.cases(obj))>0){
    obj=obj[complete.cases(obj),]
    if(verbose) message(paste("Missing values were filtered from:", path))
  }

  # Handle Time correct epoch time (Seconds and/or miliseconds)
  s=ifelse(max(as.numeric(obj[,Time.col]))>1e11,1000,1)
  obj[,Time.col]=round(obj[,Time.col]/s,0)

  TLim=c(min(as.numeric(obj[,Time.col])),max(as.numeric(obj[,Time.col])))

  if(as.Date(Sys.Date())<as.Date(as.POSIXct(TLim[2], origin = "1970-01-01", tz="UTC"))){
    if(verbose) message("The dates in the dataset are older than the current system date (UTC).")
  }

  Pos=lapply(split(obj[, c(Time.col,x.col,y.col)], obj[,ID.col]), function(d) {
    d=d[!duplicated(d[, 1]), , drop = FALSE]  # keep first occurrence per timestamp
    m=list(Time = as.integer(d[, 1] - TLim[1]),
              x = as.numeric(d[, 2]),
              y = as.numeric(d[, 3]))
  })


  # Create the AniSpace object
  obj=new("AniSpace",
      NIDs = 1:length(unique(obj[,ID.col])),
      IDs  = as.character(unique(obj[,ID.col])),
      Info = list(),
      TLim = TLim,
      TRes = TRes,
      Pos  = Pos,
      Area = list())
  return(obj)
}
