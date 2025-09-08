#' Example dataset for AniSpace: \code{cows}
#'
#' A small example dataset bundled with AniSpace to demonstrate
#' spatial analysis workflows on high-resolution position data.
#' The object is a **named list with three data frames**:
#'
#' \describe{
#'   \item{\code{positions}}{Data frame of animal positions (one row per fix).
#'     \describe{
#'       \item{\code{Date}}{Date when the information was recorded}
#'       \item{\code{Time}}{POSIXct}
#'       \item{\code{ID}}{integer or character, animal identifier}
#'       \item{\code{x}}{numeric, X coordinate (meters)}
#'       \item{\code{y}}{numeric, Y coordinate (meters)}
#'     }}
#'
#'   \item{\code{animals}}{Data frame with animal-level metadata.
#'     Typical columns:
#'     \describe{
#'       \item{\code{Date}}{Date when the information was recorded}
#'       \item{\code{ID}}{matches \code{positions$id}}
#'       \item{\code{Parity}}{integer: Number of births}
#'       \item{\code{DIM}}{integer: Days in milk}
#'       \item{\code{Breed}}{factor}
#'       \item{\code{Disease}}{Absence(0) or Presence (1)}
#'     }}
#'
#'   \item{\code{areas}}{Data frame describing the barn plan / polygons.
#'     Typical columns:
#'     \describe{
#'       \item{\code{Area}}{identifier of area}
#'       \item{\code{x1}}{numeric, X1 coordinate of vertex}
#'       \item{\code{x2}}{numeric, X2 coordinate of vertex}
#'       \item{\code{y1}}{numeric, Y1 coordinate of vertex}
#'       \item{\code{y2}}{numeric, Y2 coordinate of vertex}
#'       \item{\code{Color}}{optional, to depict polygons}
#'     }}
#' }
#'
#' @format A named list with 3 components: \code{positions}, \code{animals}, \code{areas}.
#'
#' @details
#' This dataset is intended for vignettes and examples (e.g., movement metrics,
#' data filtering, data interpolation, estimation of the accuracy of the system,
#' social network analysis, and area utilisation estimation). Coordinates are
#' assumed to be in centimeters in an indoor/local reference frame. Adjust the column
#' names/types in your build script to match your real data before saving \code{data/cows.rda}.
#'
#' @source CSI:DT project (example subset / demo build)
#'
#' @examples
#' data("cows", package = "AniSpace")
#' str(cows)
#' head(cows$positions)
#' # Example: compute a simple step length
#' with(cows$positions, head(sqrt(diff(x)^2 + diff(y)^2)))
#'
#' @keywords datasets
"cows"
