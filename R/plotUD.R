#' Plot individual spatial utilisation distributions and home ranges
#'
#' @description
#' `plotUD` plots individual spatial-utilisation distributions stored
#' in the `UD` slot of an `AniSpace` object. The spatial areas are plotted
#' first, followed by the grid-based utilisation distribution, convex polygon,
#' or kernel home-range contour estimated by `areaUD`.
#'
#' @param AniObj An object of class `AniSpace` containing spatial-utilisation
#' results in the `UD` slot.
#' @param NIDs A numeric vector indicating the numerical identifiers of the
#' individuals to plot (*Default: 1*).
#' @param IDs A character vector indicating the identifiers of the individuals
#' to plot (*Default: NULL*).
#' @param HR.level A numeric value indicating the home-range percentage contour
#' plotted when the stored method is `"HR"` (*Default: HR.level=95*).
#' @param plot.Pos A logical variable indicating whether individual positions
#' should be added to the plot (*Default: FALSE*).
#' @param grid.col A vector of colours used for grid-based utilisation
#' distributions.
#' @param verbose A logical variable indicating whether the function will print
#' relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#' @param ... Additional graphical arguments passed to `plot()`.
#'
#' @return Plot AniSpace utilisation distribution information
#'
#' @examples
#' # Filter individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=3)
#'
#' # Plot a grid-based utilisation distribution
#' df.UD=areaUD(df.ID.filt,method="grid")
#' plotUD(df.UD,NIDs=1)
#'
#' # Plot a polygon-based utilisation area
#' df.poly=areaUD(df.ID.filt,method="poly")
#' plotUD(df.poly, NIDs=1, plot.Pos=TRUE)
#'
#' # Plot a 95 percent kernel-based home range
#' df.HR=areaUD(df.ID.filt,method="HR")
#' plotUD(df.HR,NIDs=1,HR.level=95)
#'
#' @export
#'

plotUD=function(AniObj, NIDs=1, IDs=NULL,
                         HR.level=95,
                         plot.Pos=FALSE,
                         grid.col = grDevices::colorRampPalette(c("gold", "orange", "darkred"))(50),
                         verbose=TRUE,...) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.", call. = FALSE)
  if( !validate(AniObj))             stop("Invalid `AniObj` object.", call. = FALSE)

  if (length(AniObj@UD) == 0L) stop("No utilisation-distribution results were found in `AniObj`.", call. = FALSE)
  if (length(AniObj@Area) == 0L) stop("No area information was found in `AniObj`.", call. = FALSE)

  if(is.null(NIDs) & is.null(IDs)){
    i=1
  }else if(!is.null(NIDs)){
    i=NIDs
  }else if(!is.null(IDs)){
    i=which(AniObj@IDs%in%IDs)
  }
  if (!is.null(i)){ if(!any(i%in%AniObj@NIDs)) stop("Individuals not found in `AniObj`")}

  if (!is.numeric(HR.level) || length(HR.level) != 1L ||
      !is.finite(HR.level) || HR.level <= 0 || HR.level >= 100) {
    stop("`HR.level` must be a single numeric value between 0 and 100.",call. = FALSE)}

  if (!is.logical(plot.Pos) || length(plot.Pos) != 1L ||
      is.na(plot.Pos)) {stop("`plot.Pos` must be either TRUE or FALSE.", call. = FALSE)}

  if (!is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.", call. = FALSE)

  # Create plot
  coords=do.call(rbind, lapply(AniObj@Area, function(a) a$coords[, c("x","y"), drop = FALSE]))
  xlim=range(coords[, "x"], na.rm = TRUE)
  ylim=range(coords[, "y"], na.rm = TRUE)

  # Image
  plot(NA, xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y", asp=1, xaxt='n', yaxt='n',  ...)

  # Plot polygons
  for (ii in 1:length(AniObj@Area)) {
    graphics::polygon(c(AniObj@Area[[ii]]$coords[,"x"]),
                      c(AniObj@Area[[ii]]$coords[,"y"]),
                      col = AniObj@Area[[ii]]$color)}

  #---
  method=AniObj@UD$method
  UD=AniObj@UD$UD

  if (!is.character(method) || length(method) != 1L ||
      !method %in% c("grid", "poly", "HR")) {
    stop("The method stored in `AniObj@UD` is invalid.", call. = FALSE)
  }

  if (!is.list(UD) || length(UD) != length(AniObj@IDs)) {
    stop("The results stored in `AniObj@UD$UD` are invalid.",
         call. = FALSE)
  }

  #----------------------------------------------------------
  # Grid-based utilisation distribution
  #----------------------------------------------------------
  if (method == "grid") {
    xbreaks=AniObj@UD$parameters$xbreaks
    ybreaks=AniObj@UD$parameters$ybreaks

    nx=length(xbreaks) - 1L
    ny=length(ybreaks) - 1L

    M=matrix(0L, nrow = ny, ncol = nx,
      dimnames = list( y = utils::head(ybreaks, -1L), x = utils::head(xbreaks, -1L)))

    for(ii in 1:length(i)){
      if(is.null(UD[[i[ii]]])) {
        if(verbose) warning("No UD information available for individual: `", AniObj@IDs[i],"`.")
        next
      }
      M[UD[[i[ii]]]$cell]=M[UD[[i[ii]]]$cell]+UD[[i[ii]]]$z
    }

    xcenters=utils::head(xbreaks, -1L) + diff(xbreaks) / 2
    ycenters=utils::head(ybreaks, -1L) + diff(ybreaks) / 2
    Z=t(M)
    Z[Z == 0L]=NA

    if (any(is.finite(Z))) {
      graphics::image( x = xcenters, y = ycenters, z = Z,
        col = grid.col, add = TRUE)
    }
  }

  #----------------------------------------------------------
  # Polygon-based utilisation area
  #----------------------------------------------------------
  if (method == "poly") {
    Rcols=grDevices::rainbow(length(i))

    for(ii in 1:length(i)){
      if(is.null(UD[[i[ii]]])) {
        if(verbose) warning("No UD information available for individual: `", AniObj@IDs[i],"`.")
        next
      }

      if (plot.Pos) graphics::points(x = AniObj@Pos[[i[ii]]]$x, y = AniObj@Pos[[i[ii]]]$y,
                                     pch = 16, bg = Rcols[ii], col = Rcols[ii], cex=0.35)
      graphics::polygon(x = UD[[i[ii]]]$x, y = UD[[i[ii]]]$y, border = Rcols[ii], lwd = 3, col = NA)
    }
  }


  #----------------------------------------------------------
  # Kernel home range
  #----------------------------------------------------------
  if (method == "HR") {

    if(length(i)>1L){
      if(verbose) warning(
        "Only one individual can be plotted at a time when `method = \"HR\"`. The first selected individual will be plotted.",
        call. = FALSE)
      i=i[1L]
    }
    if(is.null(UD[[i]])) stop("No UD information available for individual: `", AniObj@IDs[i],"`.")

    # Contours to estimate
    Perlist=rev(seq(5, 95, 5))
    colPal=grDevices::colorRampPalette(c("lightyellow", "gold", "orange", "red", "darkred"))(length(Perlist))

    # Try to extract every contour independently
    HRlist=lapply(Perlist, function(percent) {
      tryCatch(adehabitatHR::getverticeshr(UD[[i]], percent = percent),
               error = function(e) {NULL})
    })

    available=vapply(HRlist, function(x) !is.null(x), logical(1))
    if (!any(available)) stop("No home-range contours could be extracted for ID: ", AniObj@IDs[i], ".", call. = FALSE)

    HRpoly=tryCatch(adehabitatHR::getverticeshr(UD[[i]], percent = HR.level),
              error = function(e) {
                if(verbose) warning("The ", HR.level, "% home-range contour could not be extracted for ID: ",
                        AniObj@IDs[i], " | ", conditionMessage(e), call. = FALSE)
                NULL})

    # Plot home-range contours
    for (j in which(available)) {
      sp::plot(HRlist[[j]], add = TRUE, col = colPal[j], border = NA)
    }

    if(!any(available)){
      highest.available=max(Perlist[available])
      if(verbose) warning("- Home-range contours above ", highest.available,
              "% could not be plotted for ID: ", AniObj@IDs[i],".")}

    if(!is.null(HRpoly)){
      sp::plot(HRpoly, add = TRUE, border = "black", lwd = 1)
    }
  }
}
