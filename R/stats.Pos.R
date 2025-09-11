#' Descriptive statistics of the temporal-spatial information
#'
#' @description
#' `stats.Pos` estimates the descriptive statistics of the temporal and spatial information contained within the position object at an individual level.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param graphs A logical variable indicates whether the graphs showing the descriptive statistics will be reported back (TRUE) or not (FALSE) (*Default: FALSE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords descriptive statistics spatial position information
#'
#' @return Descriptive statistics of the temporal-spatial information detailed in the AniSpace object.
#'
#' @examples
#' df.stats.Pos=stats.Pos(AniObj=df, graphs=TRUE)
#'
#' head(df.stats.Pos$DescriptiveStats)
#'
#' df.stats.Pos$graphs[[1]]
#'
#' @export
#'

stats.Pos=function(AniObj, graphs=FALSE, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")


  # Descriptive statistics
  l=lapply(seq_along(AniObj@Pos), function(i) {
    p=AniObj@Pos[[i]]
    a=length(p$Time)

    if (a < 2L) {
      return(data.frame(NIDs = i, IDs = AniObj@IDs[i], npositions = a,
                        min_x=p$x, max_x=p$x, min_y= p$y, max_y=p$y, median_x=NA, median_y=NA,
                        Time_range=NA, mean_Time_step=NA,
                        total_dist=NA ,total_area=NA,mean_speed=NA, max_speed=NA))}

    dt=diff(as.numeric(p$Time))
    dx=diff(as.numeric(p$x))
    dy=diff(as.numeric(p$y))

    dist =sqrt(dx^2 + dy^2)
    speed=dist / dt

    # guard against non-positive/NA intervals
    ok <- is.finite(speed) & is.finite(dist) & dt > 0
    return(data.frame(NIDs = i, IDs = AniObj@IDs[i], npositions = a,
                      min_x=min(p$x), max_x=max(p$x), min_y=min(p$y), max_y=max(p$y),
                      median_x=median(p$x), median_y=median(p$y),
                      Time_range=as.numeric(max(p$Time), min(p$Time)),
                      mean_Time_step=mean(dt, na.rm=TRUE),
                      total_dist=sum(dist[ok], na.rm = TRUE),
                      total_area= (max(p$x)-min(p$x))*(max(p$y)-min(p$y)),
                      mean_speed=mean(speed[ok], na.rm = TRUE),
                      max_speed=max(speed[ok], na.rm = TRUE)))
  })

  sf=do.call(rbind, l)
  row.names(sf)=NULL

  # Print global statistics
  if (verbose) message("- Global statistics:
               Average distance walked: ", round(mean(sf$total_dist, na.rm = TRUE),2), "
               Average area walked: ",     round(mean(sf$total_area, na.rm = TRUE),2), "
               Average speed: ",           round(mean(sf$mean_speed, na.rm = TRUE),2))

  # Visual representations
  if(graphs){
    dsf=list()
    dsf[["DescriptiveStats"]]=as.data.frame(sf)

    graphs=list()
    fill="#4287F578";border="#4287F5DC"

    #X-axis distribution
    v=unlist(lapply(AniObj@Pos, function(p) p$x), use.names = FALSE)
    hist(v, breaks = "FD", probability = TRUE,
         col = fill, border = border,
         main = "X-axis distribution", xlab = "x", ylab = "", las = 1)
      abline(v = mean(v), lwd = 2, lty = 2, col = "firebrick")
      abline(v = median(v), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["X-axis_Dist"]]=grDevices::recordPlot()

    #Y-axis distribution
    v=unlist(lapply(AniObj@Pos, function(p) p$y), use.names = FALSE)
    hist(v, breaks = "FD", probability = TRUE,
                                 col = fill, border = border,
                                 main = "Y-axis distribution", xlab = "y", ylab = "", las = 1)
    abline(v = mean(v), lwd = 2, lty = 2, col = "firebrick")
    abline(v = median(v), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["Y-axis_Dist"]]=grDevices::recordPlot()

    #Time distribution
    v=unlist(lapply(AniObj@Pos, function(p) p$Time), use.names = FALSE)
    hist(v, breaks = "FD", probability = TRUE,
                                 col = fill, border = border,
                                 main = "Time distribution", xlab = "Time", ylab = "", las = 1)
    abline(v = mean(v), lwd = 2, lty = 2, col = "firebrick")
    abline(v = median(v), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["Time_Dist"]]=grDevices::recordPlot()

    l=lapply(seq_along(AniObj@Pos), function(i) {
      p=AniObj@Pos[[i]]
      a=length(p$Time)

      if (a < 2L) return(data.frame(dt=NA, dist=NA, speed=NA))

      dt=diff(as.numeric(p$Time))
      dx=diff(as.numeric(p$x))
      dy=diff(as.numeric(p$y))

      dist =sqrt(dx^2 + dy^2)
      speed=dist / dt

      # guard against non-positive/NA intervals
      ok <- is.finite(speed) & is.finite(dist) & dt > 0
      return(data.frame(dt=dt,dist=dist[ok],speed=speed[ok]))
    })
    ll=do.call(rbind, l)

    #Time interval distribution
    hist(ll$dt, probability = TRUE,
                                        col = fill, border = border,
                                        main = "Time interval distribution", xlab = "Time interval", ylab = "", las = 1)
    abline(v = mean(ll$dt), lwd = 2, lty = 2, col = "firebrick")
    abline(v = median(ll$dt), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["Time_Interval_Dist"]]=grDevices::recordPlot()

    #Time interval distribution
    hist(ll$dist, probability = TRUE,
                                        col = fill, border = border,
                                        main = "Walked distance distribution", xlab = "Walked distance", ylab = "", las = 1)
    abline(v = mean(ll$dist), lwd = 2, lty = 2, col = "firebrick")
    abline(v = median(ll$dist), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["walked_dist_Dist"]]=grDevices::recordPlot()

    #Time interval distribution
    hist(ll$speed, probability = TRUE,
                                      col = fill, border = border,
                                      main = "Speed distribution", xlab = "Speed", ylab = "", las = 1)
    abline(v = mean(ll$speed), lwd = 2, lty = 2, col = "firebrick")
    abline(v = median(ll$speed), lwd = 2, lty = 3, col = "darkgreen")
    graphs[["Speed_Dist"]]=grDevices::recordPlot()

    dsf[["graphs"]]=graphs

    if (verbose) message(" - See the `obj$graphs` section for a depiction of the parameters' distribution.
  The dotted red and green lines represent the mean and median, respectively.")

  }else{
    dsf=as.data.frame(sf)
  }

  return(dsf)
}







