#' Transform areas' information to polygon structure
#'
#' @description
#' `square2poly()` tranform the information on the distribution of the barn areas to polygons structure.
#'
#' @param AreaObj A data frame containing information for the  barn areas in (x1,x2,y1,y2 format).
#' @param Area.col A character variable specifying the name of the column containing the Area identification in the file.
#' @param x1.col A character variable specifying the name of the column containing the x1 position information in the file.
#' @param x2.col A character variable specifying the name of the column containing the x2 position information in the file.
#' @param y1.col A character variable specifying the name of the column containing the y1 position information in the file.
#' @param y2.col A character variable specifying the name of the column containing the y2 position information in the file.
#'
#' @keywords transform area information
#'
#' @return list of polygons
#'
#' @examples
#' bf=read.csv("data/barnplan.csv", header=T)
#' cf=square2poly(bf)
#' head(cf,1)
#'
#' @export

square2poly <- function(AreaObj, Area.col="Area",
                        x1.col="x1", x2.col="x2", y1.col="y1", y2.col="y2") {

  # Control parameters
  if(!is.data.frame(AreaObj))         stop("`InfObj` must be a data.frame.")
  if(!all(is.character(Area.col), is.character(x1.col), is.character(x2.col), is.character(y1.col), is.character(y2.col)))
    stop("Column names (*.col) must be characters")

  # Check/Rename column names
  if(!any(Area.col==colnames(AreaObj)))  stop("Column name `Area.col` not found")
  if(!any(x1.col==colnames(AreaObj)))    stop("Column name `x1.col` not found")
  if(!any(x2.col==colnames(AreaObj)))    stop("Column name `x2.col` not found")
  if(!any(y1.col==colnames(AreaObj)))    stop("Column name `y1.col` not found")
  if(!any(y2.col==colnames(AreaObj)))    stop("Column name `y2.col` not found")
  if(!any("Color"==colnames(AreaObj))) {
    AreaObj$Color=NA
    message("A 'Color' column can be added for visualization; it is currently filled with NA values.")
  }

  # Transform data to polygons
  obj=lapply(split(AreaObj, 1:nrow(AreaObj)), function(d) {
    res=lapply(seq_len(nrow(d)), function(i) {
      coords=cbind(
        x = c(d[[x1.col]][i], d[[x2.col]][i], d[[x2.col]][i], d[[x1.col]][i], d[[x1.col]][i]),
        y = c(d[[y1.col]][i], d[[y1.col]][i], d[[y2.col]][i], d[[y2.col]][i], d[[y1.col]][i])
      )
      list(ID     =  as.character(d[[Area.col]][i]),
           coords = coords,
           color  = if ("Color" %in% names(d)) d[["Color"]][i] else "grey")
    })
    if (length(res) == 1L) res[[1L]] else res
  })

  return(obj)
}



