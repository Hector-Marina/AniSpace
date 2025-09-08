#' Estimate accuracy of the positioning system
#'
#' @description
#' `stand.still` estimate accuracy of the positioning system using stand-still devices
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param NIDs Numeric. Vector containing the assigned index of the individuals (*Default: NULL*).
#' @param IDs  Character. Vector containing the identification of the individuals (*Default: NULL*).
#' @param percentile A numeric variable indicating the percentiles to be evaluated (Default: *percentile=0.99*).
#' @param graphs A logical variable indicates whether the filtered position will be depicted by individual (TRUE) or not (FALSE) (*Default: FALSE*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords accuracy system estimation stand-still devices
#'
#' @return Estimated accuracy of the system
#'
#' @examples
#' # Estimate accuracy of the positioning system using stand-still devices
#' StandStill.acc=stand.still(df,keep. NIDs=c(1:10))
#'
#' @export

stats.still=function(AniObj, NIDs=NULL, IDs=NULL, percentile=0.99, graphs=FALSE, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (is.null(NIDs) && is.null(IDs)) {
    stop("Stand-still devices need to be defined using the parameters `NIDs` or `IDs` ")
  } else if (is.null(NIDs) & !is.null(IDs)) {
    NIDs=which(AniObj@IDs %in% IDs)
  }
  if(!any(NIDs%in%AniObj@NIDs))stop("Individuals not found in `AniObj`")

  if(!is.numeric(percentile))  stop("`percentile` must be numeric")
  if(!is.logical(graphs))      stop("`graphs` must be logical")
  if(!is.logical(verbose))     stop("`verbose` must be logical")

  # Extract information from AniSpace object
  o=do.call(rbind, lapply(NIDs, function(i) {
    p =AniObj@Pos[[i]]
    a =length(p$Time)
    mx=mean(p$x, na.rm = TRUE); my=mean(p$y, na.rm = TRUE)
    d =sqrt((p$x - mx)^2 + (p$y - my)^2)

    out=data.frame(
      NIDs        = i,
      IDs         = AniObj@IDs[i],
      npositions  = a,
      cent_x      = mx,
      cent_y      = my,
      mean_error  = mean(d, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    qv=as.numeric(quantile(d, probs = percentile, na.rm = TRUE, names = FALSE))
    qn=paste0("q", gsub("\\.?0+$", "", format(100 * percentile, trim = TRUE)))
    out[qn]=qv
    out
  }))
  row.names(o)=NULL


  if(verbose){
    for (j in 1:length(percentile)){
      message(paste("- Average distance error for",percentile[j], "percentile was", round(mean(o[,6+j]),2) ))
    }
  }

  # Visual representations
  if(graphs){
    stats=list()
    stats[["stats"]]=as.data.frame(o)
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

    # Plot positions
    for (ii in 1:length(NIDs)){
      points(x = AniObj@Pos[[ii]]$x, y = AniObj@Pos[[ii]]$y, col = "orange", cex=0.5)
    }

    # Plot radious

    if (length(percentile)==1){Rcols="blue"
    } else {Rcols=grDevices::colorRampPalette(c("blue", "purple"))(length(percentile))
    }
    for (i in 1:nrow(o)){
      for (j in 1:length(percentile)){
        symbols(o[i,4],o[i,5],
                circles = o[i,(6+j)],
                inches = FALSE, add = TRUE,
                fg = Rcols[j], lwd = 1.5)
      }
    }
    stats[["graphs"]]=grDevices::recordPlot()
  }else{
    stats=as.data.frame(o)
  }

  return(stats)
}
