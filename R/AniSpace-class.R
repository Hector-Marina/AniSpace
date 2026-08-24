#' AniSpace class
#'
#' An S4 class representing the spatial and temporal information of multiple
#' individuals, together with optional spatial-interaction and
#' utilisation-distribution analyses.
#'
#' @slot NIDs Numeric vector containing the numerical index assigned to each
#' individual.
#' @slot IDs Character vector containing the individual identifiers.
#' @slot Info List containing individual-level information. Elements should
#' generally have one value per individual.
#' @slot TLim Numeric vector of length two containing the minimum and maximum
#' temporal limits covered by the positions.
#' @slot TRes Numeric value containing the temporal resolution of the positions,
#' expressed in the units used by the object.
#' @slot Pos List containing the spatial positions of each individual. Each
#' element corresponds to one individual.
#' @slot Area List containing the spatial area available to each individual,
#' generally represented by polygon coordinates.
#' @slot SI List containing pairwise spatial-interaction results. An empty list
#' indicates that no spatial-interaction analysis has been performed. Otherwise,
#' the list contains:
#'
#' * `method`: interaction method (`"time"`, `"nint"`, or `"all"`).
#' * `dist.thr`: optional spatial-distance threshold.
#' * `time.thr`: optional temporal threshold.
#' * `M`: square pairwise spatial-interaction matrix.
#'
#' @slot UD List containing utilisation-distribution results. An empty list
#' indicates that no utilisation distribution has been estimated. Otherwise,
#' the list contains:
#'
#' * `method`: utilisation method (`"grid"`, `"poly"`, or `"HR"`).
#' * `parameters`: list of parameters used to estimate the distribution,
#'   including the spatial grid limits when applicable.
#' * `UD`: list containing one utilisation-distribution result per individual.
#'
#' For `"grid"`, each valid element of `UD` is a data frame containing `cell`
#' and `z`. For `"poly"`, each valid element is a closed polygon represented by
#' `x` and `y` coordinates. For `"HR"`, each valid element is an `estUD` object.
#' Individuals without sufficient information are represented by `NULL`.
#'
#' @slot UDsim List containing pairwise utilisation-distribution similarities.
#' An empty list indicates that no similarity analysis has been performed.
#' Otherwise, the list contains:
#'
#' * `method`: utilisation method associated with the comparison.
#' * `sim.method`: similarity method (`"binary"` or `"continuous"`).
#' * `HR.level`: probability contour used for kernel home-range comparisons.
#' * `M`: square pairwise similarity matrix with values between 0 and 1.
#'
#' Rows of `M` represent focal individuals and columns represent the individuals
#' against which their utilisation is compared. The matrix may therefore be
#' directional. Continuous kernel home-range similarity is symmetric.
#'
#' @examples
#' # Create an object without derived analyses
#' AniObj=new(
#'   "AniSpace",
#'   NIDs=c(1,2),
#'   IDs=c("A","B"),
#'   Info=list(Trait1=c(0.2,0.8)),
#'   TLim=as.numeric(as.POSIXct(
#'     c("2021-01-01","2021-01-02"),tz="UTC")),
#'   TRes=1,
#'   Pos=list(
#'     A=matrix(runif(20),ncol=2,
#'       dimnames=list(NULL,c("x","y"))),
#'     B=matrix(runif(20),ncol=2,
#'       dimnames=list(NULL,c("x","y")))
#'   ),
#'   Area=list(),
#'   SI=list(),
#'   UD=list(),
#'   UDsim=list()
#' )
#'
#' @export
methods::setClass(
  "AniSpace",
  slots=c(
    NIDs="numeric",
    IDs="character",
    Info="list",
    TLim="numeric",
    TRes="numeric",
    Pos="list",
    Area="list",
    SI="list",
    UD="list",
    UDsim="list"
  ),
  prototype=list(
    NIDs=numeric(),
    IDs=character(),
    Info=list(),
    TLim=numeric(),
    TRes=numeric(),
    Pos=list(),
    Area=list(),
    SI=list(),
    UD=list(),
    UDsim=list()
  )
)
