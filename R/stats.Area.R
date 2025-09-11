#' Descriptive statistics of the facility drawings
#'
#' @description
#' `stats.Area` estimates the descriptive statistics of the areas detailed in the AniSpace object.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param graphs A logical variable indicates whether the graphs showing the descriptive statistics will be reported back (TRUE) or not (FALSE) (*Default: FALSE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords descriptive statistics area information
#'
#' @return Descriptive statistics of the areas detailed in the AniSpace object.
#'
#' @examples
#' df.stats.Area=stats.Area(AniObj=df, graphs=FALSE)
#'
#' head(df.stats.Area$DescriptiveStats)
#'
#' df.stats.Area$graphs[[1]]
#'
#' @export

stats.Area <- function(AniObj, graphs=FALSE, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  # Descriptive statistics
  l=lapply(seq_along(AniObj@Area), function(i) {
    coords=AniObj@Area[[i]][["coords"]]

    v=1:(nrow(coords) - 1)
    area=0.5 * abs(sum(coords[v, 1] * coords[v + 1, 2] - coords[v + 1, 1] * coords[v, 2]))

    return(data.frame(NArea = i, Area = AniObj@Area[[i]]$ID,
                      mean_x=mean(coords[, 1]), range_x=as.numeric(max(coords[, 1]), min(coords[, 1])),
                      mean_y=mean(coords[, 2]), range_y=as.numeric(max(coords[, 2]), min(coords[, 2])),
                      area= area))
  })

  pf=do.call(rbind, l)
  row.names(pf)=NULL

  pf$relative_area=pf$area/sum(pf$area)

  # Occupation
  vx=unlist(lapply(AniObj@Pos, function(p) p$x), use.names = FALSE)
  vy=unlist(lapply(AniObj@Pos, function(p) p$y), use.names = FALSE)

  # Descriptive statistics
  o = sapply(seq_along(AniObj@Area), function(i) {
    P=AniObj@Area[[i]]$coords
    if (!all(P[1, ] == P[nrow(P), ])) P=rbind(P, P[1, ])
    n=nrow(P) - 1L; IN=ED=logical(length(vx)); eps=1e-9
    for (j in 1:n) {
      x1=P[j,1]; y1=P[j,2]; x2=P[j+1,1]; y2=P[j+1,2]
      ED=ED | (abs((vx - x1)*(y2 - y1) - (vy - y1)*(x2 - x1)) < eps &
                    vx >= pmin(x1, x2) - eps & vx <= pmax(x1, x2) + eps &
                    vy >= pmin(y1, y2) - eps & vy <= pmax(y1, y2) + eps)
      IN=xor(IN, ((y1 > vy) != (y2 > vy)) &
                  (vx < (x2 - x1) * (vy - y1) / (y2 - y1) + x1))
    }
    sum(IN | ED)
  })

  pf$occupation=o
  pf$density=o/pf$area

  # Print global statistics
  if (verbose) message("- Global statistics:
               Average area size: ", round(mean(pf$area, na.rm = TRUE),2), "
               Average occupation: ",round(mean(pf$occupation, na.rm = TRUE),2), "
               Average density: ",   round(mean(pf$density, na.rm = TRUE),2))

  # Visual representations
  if(graphs){
    dpf=list()
    dpf[["DescriptiveStats"]]=as.data.frame(pf)
    graphs=list()

    # Create plot limited by the areas
    coords=do.call(rbind, lapply(AniObj@Area, function(a) a$coords[, c("x","y"), drop = FALSE]))
    xlim=range(coords[, "x"], na.rm = TRUE)
    ylim=range(coords[, "y"], na.rm = TRUE)

    # Image
    plot(NA, xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y", asp=1, xaxt='n')

    # Plot polygons
    for (jj in 1:length(AniObj@Area)) {
      polygon(c(AniObj@Area[[jj]]$coords[,"x"]),
              c(AniObj@Area[[jj]]$coords[,"y"]),
              col = AniObj@Area[[jj]]$color)}
    graphs[["Area"]]=grDevices::recordPlot()

  }else{
    dpf=as.data.frame(pf)
  }

  return(dpf)
}







