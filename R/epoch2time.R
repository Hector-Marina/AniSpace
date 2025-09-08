#' Convert epoch time into a string representation
#'
#' @description
#' `epoch2time` converts durations expressed in seconds (e.g., epoch differences)
#' into seconds, minutes, hours, or days depending on magnitude.
#'
#' @param x Numeric vector of durations in seconds.
#'
#' @return A character string of the same length, with each element
#'   formatted in the most suitable unit (seconds, minutes, hours, or days).
#'
#' @examples
#' epoch2time(45)      # "45 sec"
#' epoch2time(120)     # "2.0 min"
#' epoch2time(5400)    # "1.5 hours"
#' epoch2time(86400)   # "1.0 days"
#'
#' @export

epoch2time <- function(x) {
  if (!is.numeric(x)) stop("`x` must be numeric (seconds).")
  if (length(x) == 0L) return(character())

  # Work on absolute values to choose units; remember signs to prefix later
  sign_x = sign(x)
  ax = abs(x)

  out = rep(NA_character_, length(ax))

  is_na   = is.na(ax)
  is_sec  = !is_na & ax < 60
  is_min  = !is_na & ax >= 60    & ax < 3600
  is_hour = !is_na & ax >= 3600  & ax < 86400
  is_day  = !is_na & ax >= 86400

  out[is_sec]  = sprintf("%.0f sec",   ax[is_sec])
  out[is_min]  = sprintf("%.1f min",   ax[is_min]  / 60)
  out[is_hour] = sprintf("%.1f hour(s)", ax[is_hour] / 3600)
  out[is_day]  = sprintf("%.1f day(s)",  ax[is_day]  / 86400)

  # Reapply sign for negative durations
  neg = !is_na & sign_x < 0
  out[neg] = paste0("-", out[neg])

  return(out)
}
