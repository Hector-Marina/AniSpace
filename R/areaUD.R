#' Estimate individual spatial utilisation distributions and home ranges
#'
#' @description
#' `areaUD` estimates the spatial utilisation of each individual from
#' the position information stored in an `AniSpace` object. Depending on the
#' selected method, the function returns a grid-based utilisation distribution,
#' a polygon-based utilisation area, or a kernel-based home-range estimate.
#'
#' @param AniObj An object of class `AniSpace` containing the individuals' spatio-temporal position information.
#' @param method A character value specifying the spatial-utilisation approach. Available options are "grid", "poly", and "HR".
#' `grid` estimates a grid-based utilisation distribution for each individual.
#' `poly` estimates polygon-based utilisation areas from the observed positions.
#' `HR` estimates individual home ranges using kernel utilisation distributions (Default: *method="HR"*).
#' @param res A positive numeric vector of length two specifying the spatial resolution in the x and y directions, respectively.
#' Values must use the same spatial units as the coordinates stored in `AniObj`. (*Default: c(100,100)*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @return The modified `AniSpace` object with the estimated utilisation distributions or home-range results stored in the `UD` slot.
#'
#' @examples
#' # Filter individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=c(3:7))
#'
#' # Estimate kernel-based home ranges
#' df.HR  =areaUD(df.ID.filt, method = "HR")
#'
#' # Estimate grid-based utilisation distributions
#' df.UD  =areaUD(df.ID.filt, method = "grid")
#'
#' # Estimate polygon-based utilisation areas
#' df.poly=areaUD(df.ID.filt, method = "poly")
#'
#' @export
#'

areaUD=function(AniObj, method="HR", res=c(100,100), verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !method %in% c("grid", "poly","HR")) {
    stop("Invalid `method`: must be one of 'grid', 'poly' or 'HR'.")
  }

  if (!is.numeric(res)     || length(res) != 2L ||
      any(!is.finite(res)) || any(res <= 0))  stop("`res` must contain two positive finite numeric values.")

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

  if(length(AniObj@UDsim)>0L){
    if(verbose)
      warning("Resetting previously estimated utilisation-distribution similarity information.")
    AniObj@UDsim=list()
  }
  # Inferring area information if not available
  if (length(AniObj@Area) == 0L) {
    warning( "No area information was found. Inferring area boundaries from position data.")

    xlim=range(unlist(lapply(AniObj@Pos, function(p) p$x),use.names = FALSE))
    ylim=range(unlist(lapply(AniObj@Pos, function(p) p$y),use.names = FALSE))

    AniObj=load.Area(AniObj=AniObj, square2poly(data.frame(Area=0,
                                                        x1=xlim[1L],x2=xlim[2L],
                                                        y1=ylim[1L],y2=ylim[2L],
                                                        Color="white")))
  } else {
    xlim=range(unlist(lapply(AniObj@Pos, function(p) p$x),use.names = FALSE))
    ylim=range(unlist(lapply(AniObj@Pos, function(p) p$y),use.names = FALSE))

    coords=do.call(rbind, lapply(seq_along(AniObj@Area), function(ii) AniObj@Area[[ii]]$coords[, c("x","y"), drop = FALSE]))

    if(any(xlim[1L] < min(coords[,"x"]) | xlim[2L] > max(coords[,"x"]) |
           ylim[1L] < min(coords[,"y"]) | ylim[2L] > max(coords[,"y"]))) stop("Position data outside the area boundaries were found.")
  }

  # Grid-cell boundaries
  coords=do.call(rbind, lapply(seq_along(AniObj@Area), function(ii) AniObj@Area[[ii]]$coords[, c("x","y"), drop = FALSE]))
  xlim=range(coords[, "x"], na.rm = TRUE)
  ylim=range(coords[, "y"], na.rm = TRUE)

  xbreaks=seq(floor(  xlim[1L] / res[1L]) * res[1L],
              ceiling(xlim[2L] / res[1L]) * res[1L], by = res[1L])

  ybreaks=seq(floor(ylim[1L] / res[2L]) * res[2L],
              ceiling(ylim[2L] / res[2L]) * res[2L],by = res[2L])

  #----------------------------------------------------------
  # Grid-based utilisation distribution
  #----------------------------------------------------------
  if(method=="grid"){
    if (verbose) message("- Estimating grid-based utilisation distributions.")

    nx=length(xbreaks) - 1L
    ny=length(ybreaks) - 1L

    # Grid count
    l=lapply(seq_along(AniObj@Pos), function(i) {
      p=AniObj@Pos[[i]]

      xbin=findInterval(p$x,xbreaks,all.inside = TRUE)
      ybin=findInterval(p$y,ybreaks,all.inside = TRUE)

      cell  =ybin + (xbin - 1L) * ny
      z=table(cell)

      data.frame(
        cell = as.integer(names(z)),
        z = as.integer(z)
      )
    })
  }


  #----------------------------------------------------------
  # Polygon-based utilisation area
  #----------------------------------------------------------
  if(method=="poly"){
    if (verbose) message("- Estimating polygon-based utilisation areas.")

    l=lapply(seq_along(AniObj@Pos), function(i) {
      p=AniObj@Pos[[i]]
      xy=unique(data.frame(x = p$x,y = p$y))

      if (nrow(xy) < 3L){
        warning("Estimation failed for ID: ", AniObj@IDs[i], " | due to it contains less than three locations.")
        return(NULL)
        }

      # Convex-hull vertices
      idx =grDevices::chull(xy$x, xy$y)
      poly=xy[idx, , drop = FALSE]

      # Close the polygon
      poly=rbind(poly, poly[1L, , drop = FALSE])

      return(poly)
    })
  }

  #----------------------------------------------------------
  # Kernel home range
  #----------------------------------------------------------
  href=NULL
  if(method=="HR"){
    if (verbose) message("- Estimating individuals' home ranges.")

    # kernelUD requires square grid cells
    if (!isTRUE(all.equal(res[1L], res[2L]))) stop("`res` must contain equal x and y resolutions when `method = \"HR\"`.")

    # Prepare grid
    coords=do.call(rbind, lapply(seq_along(AniObj@Area), function(ii) AniObj@Area[[ii]]$coords[, c("x","y"), drop = FALSE]))
    xlim=range(coords[, "x"], na.rm = TRUE)
    ylim=range(coords[, "y"], na.rm = TRUE)

    # Soft push boundaries if over position with individuals' location
    pxlim=range(unlist(lapply(AniObj@Pos, function(p) p$x),use.names = FALSE))
    pylim=range(unlist(lapply(AniObj@Pos, function(p) p$y),use.names = FALSE))

    if(xlim[1L] == pxlim[1L]) xlim[1L]=xlim[1L]-1
    if(xlim[2L] == pxlim[2L]) xlim[2L]=xlim[2L]+1
    if(ylim[1L] == pylim[1L]) ylim[1L]=ylim[1L]-1
    if(ylim[2L] == pylim[2L]) ylim[2L]=ylim[2L]+1

    xseq=seq(xlim[1],xlim[2],by=res[1])
    yseq=seq(ylim[1],ylim[2],by=res[2])

    UDgrid=expand.grid(x=xseq,y=yseq)
    sp::coordinates(UDgrid)= c("x", "y")
    sp::gridded(UDgrid) =TRUE


    bound=structure(list(x = c(xlim[1L],              xlim[1L], xlim[2L], xlim[2L], xlim[1L], xlim[1L]),
                         y = c((ylim[2L]+ylim[1L])/2, ylim[2L], ylim[2L], ylim[1L], ylim[1L], ylim[2L])),
                    .Names = c("x", "y"))
    bound=do.call("cbind",bound)
    Sli  =sp::Lines(sp::Line(bound), ID="bound")
    bound=sp::SpatialLines(list(Sli))

    # Estimate href per individual
    hmax=min(diff(xlim), diff(ylim)) / 3
    href=vapply(AniObj@Pos, function(p) {
      if (length(p$x) < 5L) return(NA_real_)
      sqrt(0.5 * ( stats::var(p$x) + stats::var(p$y)) ) * length(p$x)^(-1 / 6)
    },numeric(1))
    href=ifelse(href>hmax,hmax,href)

    # KernelUD requires fine grids
    valid.href=is.finite(href) & href > 0
    if (any(valid.href) && res[1L] > min(href[valid.href])) {
        warning("The grid resolution is coarse relative to the estimated bandwidths: ",res[1L], " vs. ", min(href, na.rm = TRUE),
                "Using a finer grid may be necessary.",call. = FALSE)
    }

    # Estimate HomeRange
    l = lapply(seq_along(AniObj@Pos), function(i) {
      if (length(AniObj@Pos[[i]]$x) < 5L || !is.finite(href[i])) return(NULL)

      xy = sp::SpatialPoints(cbind(x = AniObj@Pos[[i]]$x,y = AniObj@Pos[[i]]$y))

      tryCatch(adehabitatHR::kernelUD(xy = xy, h = href[i], grid = UDgrid, kern = "bivnorm", boundary = bound),
        error = function(e) {
          warning("Home-range estimation failed for ID: ", AniObj@IDs[i], " | ", conditionMessage(e))
          return(NULL)
        }
      )
    })
  }

  names(l)=AniObj@IDs

  AniObj@UD=list(
    method = method,
    parameters = list(href=href, xbreaks=xbreaks, ybreaks=ybreaks),
    UD = l)

  # Validate filtered AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("The spatial-interaction results produced an invalid `AniObj` object.")

  return(AniObj)
}
