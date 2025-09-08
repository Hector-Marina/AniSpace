#' AniSpace class
#'
#' An S4 class to represent the spatio-temporal data of multiple individuals.
#'
#' @slot NIDs Numeric. Vector containing the assigned index of the individuals.
#' @slot IDs  Character. Vector containing the identification of the individuals.
#' @slot Info List. Indexed information about the individuals.
#' @slot TLim Numeric. Contains information about the temporal range covered by the positions (Index=1s).
#' @slot TRes Numeric. Maximum temporal resolution of the positions (e.g. smallest time step).
#' @slot Pos  List. Indexed positions (x,y,z) of the individuals.
#' @slot Area List. Information about the spatial area available for the individuals (Polygon format).
#'
#' @examples
#' new("AniSpace",
#'     NIDs = c(1,2),
#'     IDs  = c("A","B"),
#'     Info = list(Trait1 = runif(2), Trait2 = runif(2)),
#'     TLim = as.numeric(as.POSIXct(c("2021-01-01","2021-01-02"), tz="UTC")),
#'     TRes = 1,
#'     Pos  = list(as.matrix(data.frame(x = runif(43200), y = runif(43200))),
#'                 as.matrix(data.frame(x = runif(43200), y = runif(43200)))),
#'     Area = list(A = as.matrix(data.frame(x = runif(5), y = runif(5))),
#'                 B = as.matrix(data.frame(x = runif(5), y = runif(5))))))
#'
setClass(
  "AniSpace",
  slots = list(
    NIDs     = "numeric",
    IDs      = "character",
    Info     = "list",
    TLim     = "numeric",
    TRes     = "numeric",
    Pos      = "list",
    Area     = "list"
  )
)



