#' Descriptive statistics of utilisation distributions
#'
#' @description
#' `stats.UD` estimates individual-level descriptive statistics from the
#' utilisation distributions stored in an `AniSpace` object. The statistics
#' reported depend on whether the utilisation distribution was estimated using
#' grid cells, polygons, or kernel home ranges.
#'
#' @param AniObj An `AniSpace` object containing utilisation distributions
#' previously estimated with `areaUD`.
#' @param HR.levels A numeric vector containing the probability contours for
#' which kernel home-range statistics are calculated. This argument is only
#' used when the stored utilisation method is `"HR"`
#' (*Default: c(50,95)*).
#' @param verbose A logical value specifying whether informative messages are
#' printed (*Default: TRUE*).
#'
#' @return Descriptive statistics of the Utilisation Distribution information
#' detailed in the `AniSpace` object.
#'
#' For grid-based distributions, the output includes the number and percentage
#' of occupied cells, the total number of locations, and descriptive statistics
#' of cell occupancy.
#'
#' For polygon-based distributions, the output includes polygon area,
#' perimeter, number and percentage of enclosed grid cells, and polygon
#' compactness.
#'
#' For kernel home ranges, separate area, perimeter, cell-count, and retained
#' probability columns are produced for every value supplied in `HR.levels`.
#'
#' @examples
#' # Filter individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=c(1:7))
#'
#' # Descriptive statistics for a grid-based utilisation distribution
#' df.UD=areaUD(df.ID.filt,method="grid",verbose=FALSE)
#' df.UD.stats=stats.UD(df.UD,verbose=FALSE)
#' head(df.UD.stats)
#'
#' # Descriptive statistics for a polygon-based utilisation distribution
#' df.poly=areaUD(df.ID.filt,method="poly",verbose=FALSE)
#' df.poly.stats=stats.UD(df.poly,verbose=FALSE)
#' head(df.poly.stats)
#'
#' # Descriptive statistics for kernel-based home ranges
#' df.HR=areaUD(df.ID.filt,method="HR",verbose=FALSE)
#' df.HR.stats=stats.UD(df.HR,HR.levels=c(50,75,95),verbose=FALSE)
#' head(df.HR.stats)
#'
#' @export
#'
stats.UD=function(AniObj,HR.levels=c(50,95),verbose=TRUE){

  # Control parameters
  if(!inherits(AniObj,"AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if(!validate(AniObj)) stop("Invalid `AniObj` object.")

  if(length(AniObj@UD)==0L)
    stop("No utilisation-distribution results were found in `AniObj`.",call.=FALSE)

  if(!is.logical(verbose) || length(verbose)!=1L || is.na(verbose))
    stop("`verbose` must be either TRUE or FALSE.")

  method=AniObj@UD$method
  UD=AniObj@UD$UD

  if(!method %in% c("grid","poly","HR"))
    stop("The method stored in `AniObj@UD` is invalid.",call.=FALSE)

  if(method=="HR"){
    if(!is.numeric(HR.levels) || length(HR.levels)==0L ||
       any(!is.finite(HR.levels)) || any(HR.levels<=0 | HR.levels>=100))
      stop("`HR.levels` must contain numeric values between 0 and 100.",
           call.=FALSE)

    HR.levels=sort(unique(HR.levels))
  }

  # Available grid information
  if(method %in% c("grid","poly")){
    xbreaks=AniObj@UD$parameters$xbreaks
    ybreaks=AniObj@UD$parameters$ybreaks

    n.cells.available=(length(xbreaks)-1L)*(length(ybreaks)-1L)
    area.available=(max(xbreaks)-min(xbreaks))*
      (max(ybreaks)-min(ybreaks))
  }

  # Required functions
  level.tag=function(x)
    gsub("\\.","_",format(x,trim=TRUE,scientific=FALSE))

  poly.area=function(x,y){
    if(length(x)<3L) return(NA_real_)
    if(x[1L]!=x[length(x)] || y[1L]!=y[length(y)]){
      x=c(x,x[1L])
      y=c(y,y[1L])
    }
    0.5*abs(sum(x[-length(x)]*y[-1L]-
                  x[-1L]*y[-length(y)]))
  }

  poly.perimeter=function(x,y){
    if(length(x)<2L) return(NA_real_)
    if(x[1L]!=x[length(x)] || y[1L]!=y[length(y)]){
      x=c(x,x[1L])
      y=c(y,y[1L])
    }
    sum(sqrt(diff(x)^2+diff(y)^2))
  }

  HR.poly.stats=function(x,level){
    p=try(suppressWarnings(
      adehabitatHR::getverticeshr(x,percent=level)),silent=TRUE)

    if(inherits(p,"try-error") ||
       !inherits(p,"SpatialPolygonsDataFrame") ||
       length(p@polygons)==0L)
      return(c(area=NA_real_,perimeter=NA_real_,patches=NA_real_))

    area=sum(vapply(p@polygons, function(a) a@area, numeric(1)),na.rm=TRUE)

    perimeter=sum(vapply(p@polygons,function(a){
      sum(vapply(a@Polygons,function(b){
        co=b@coords

        if(nrow(co)<2L) return(0)

        if(!isTRUE(all.equal(co[1L,],co[nrow(co),])))
          co=rbind(co,co[1L,])

        sum(sqrt(rowSums(diff(co)^2)))
      },numeric(1)),na.rm=TRUE)
    },numeric(1)),na.rm=TRUE)

    patches=sum(vapply(p@polygons, function(a) length(a@Polygons), integer(1)))

    c(area=area, perimeter=perimeter, patches=patches)
  }

  #----------------------------------------------------------
  # Grid-based utilisation distribution
  #----------------------------------------------------------
  if(method=="grid"){
    if(verbose)
      message("- Estimating grid-based utilisation-distribution statistics.")

    l=lapply(seq_along(UD),function(i){
      x=UD[[i]]

      if(is.null(x)){
        return(data.frame(
          NIDs=AniObj@NIDs[i],IDs=AniObj@IDs[i],method=method,
          n_positions=NA_real_,
          n_cells_occupied=NA_integer_,
          n_cells_available=n.cells.available,
          pct_cells_occupied=NA_real_,
          mean_occupancy=NA_real_,
          sd_occupancy=NA_real_,
          min_occupancy=NA_real_,
          q25_occupancy=NA_real_,
          median_occupancy=NA_real_,
          q75_occupancy=NA_real_,
          max_occupancy=NA_real_,
          max_cell_percentage=NA_real_))
      }

      z=x$z[is.finite(x$z) & x$z>0]

      if(length(z)==0L){
        return(data.frame(
          NIDs=AniObj@NIDs[i],IDs=AniObj@IDs[i],method=method,
          n_positions=0,
          n_cells_occupied=0L,
          n_cells_available=n.cells.available,
          pct_cells_occupied=0,
          mean_occupancy=NA_real_,
          sd_occupancy=NA_real_,
          min_occupancy=NA_real_,
          q25_occupancy=NA_real_,
          median_occupancy=NA_real_,
          q75_occupancy=NA_real_,
          max_occupancy=NA_real_,
          max_cell_percentage=NA_real_))
      }

      q=stats::quantile(z,c(0.25,0.5,0.75),
                        names=FALSE,na.rm=TRUE)

      data.frame(
        NIDs=AniObj@NIDs[i],
        IDs=AniObj@IDs[i],
        method=method,
        n_positions=sum(z),
        n_cells_occupied=length(z),
        n_cells_available=n.cells.available,
        pct_cells_occupied=100*length(z)/n.cells.available,
        mean_occupancy=mean(z),
        sd_occupancy=if(length(z)>1L) stats::sd(z) else NA_real_,
        min_occupancy=min(z),
        q25_occupancy=q[1L],
        median_occupancy=q[2L],
        q75_occupancy=q[3L],
        max_occupancy=max(z),
        max_cell_percentage=100*max(z)/sum(z))
    })
  }

  #----------------------------------------------------------
  # Polygon-based utilisation distribution
  #----------------------------------------------------------
  if(method=="poly"){
    if(verbose)
      message("- Estimating polygon-based utilisation-distribution statistics.")

    G=expand.grid(
      x=utils::head(xbreaks,-1L)+diff(xbreaks)/2,
      y=utils::head(ybreaks,-1L)+diff(ybreaks)/2)

    l=lapply(seq_along(UD),function(i){
      p=UD[[i]]

      if(is.null(p)){
        return(data.frame(
          NIDs=AniObj@NIDs[i],IDs=AniObj@IDs[i],method=method,
          n_vertices=NA_integer_,
          area=NA_real_,
          perimeter=NA_real_,
          compactness=NA_real_,
          n_cells_polygon=NA_integer_,
          n_cells_available=n.cells.available,
          pct_cells_polygon=NA_real_,
          area_available=area.available,
          pct_area_available=NA_real_))
      }

      area=poly.area(p$x,p$y)
      perimeter=poly.perimeter(p$x,p$y)

      inside=sp::point.in.polygon(
        G$x,G$y,p$x,p$y)>0

      closed=isTRUE(all.equal(p$x[1L],p$x[nrow(p)])) &&
        isTRUE(all.equal(p$y[1L],p$y[nrow(p)]))

      data.frame(
        NIDs=AniObj@NIDs[i],
        IDs=AniObj@IDs[i],
        method=method,
        n_vertices=nrow(p)-as.integer(closed),
        area=area,
        perimeter=perimeter,
        compactness=if(is.finite(area) && is.finite(perimeter) &&
                       perimeter>0) 4*pi*area/perimeter^2 else NA_real_,
        n_cells_polygon=sum(inside),
        n_cells_available=n.cells.available,
        pct_cells_polygon=100*sum(inside)/n.cells.available,
        area_available=area.available,
        pct_area_available=100*area/area.available)
    })
  }

  #----------------------------------------------------------
  # Kernel home range
  #----------------------------------------------------------
  if(method=="HR"){
    if(verbose)
      message("- Estimating kernel home-range statistics.")

    l=lapply(seq_along(UD),function(i){
      x=UD[[i]]

      out=list(
        NIDs=AniObj@NIDs[i],
        IDs=AniObj@IDs[i],
        method=method,
        n_cells_available=NA_integer_,
        area_available=NA_real_)

      for(level in HR.levels){
        tag=level.tag(level)
        out[[paste0("n_cells_HR",tag)]]=NA_integer_
        out[[paste0("pct_cells_HR",tag)]]=NA_real_
        out[[paste0("area_HR",tag)]]=NA_real_
        out[[paste0("perimeter_HR",tag)]]=NA_real_
        out[[paste0("discc_patches",tag)]]=NA_integer_
      }

      if(is.null(x))
        return(as.data.frame(out,check.names=FALSE))

      d=as.data.frame(x)
      v=as.data.frame(adehabitatHR::getvolumeUD(x))

      colnames(d)=colnames(v)=c("ud","x","y")

      d=d[order(d$x,d$y),]
      v=v[order(v$x,v$y),]

      gp=sp::gridparameters(x)
      cell.area=prod(gp[,2])

      out$n_cells_available=nrow(d)
      out$area_available=nrow(d)*cell.area


      for(level in HR.levels){
        tag=level.tag(level)
        use=is.finite(v$ud) & v$ud<=level
        pstats=HR.poly.stats(x,level)

        if(any(is.na(pstats))){
          out[[paste0("n_cells_HR",tag)]]=NA_integer_
          out[[paste0("pct_cells_HR",tag)]]=NA_real_
          out[[paste0("area_HR",tag)]]=NA_real_
          out[[paste0("perimeter_HR",tag)]]=NA_real_
          out[[paste0("discc_patches",tag)]]=NA_integer_
        }else{
          out[[paste0("n_cells_HR",tag)]]=sum(use)
          out[[paste0("pct_cells_HR",tag)]]=100*sum(use)/nrow(d)
          out[[paste0("area_HR",tag)]]=unname(pstats["area"])
          out[[paste0("perimeter_HR",tag)]]=unname(pstats["perimeter"])
          out[[paste0("discc_patches",tag)]]=as.integer(pstats["patches"])
        }
      }

      as.data.frame(out,check.names=FALSE)
    })
  }

  sf=do.call(rbind,l)
  row.names(sf)=NULL

  # Global statistics
  if(verbose && method=="grid")
    message("- Global statistics:
             Average occupied cells: ",
            round(mean(sf$n_cells_occupied,na.rm=TRUE),2),"
             Average occupied-cell percentage: ",
            round(mean(sf$pct_cells_occupied,na.rm=TRUE),2),"%")

  if(verbose && method=="poly")
    message("- Global statistics:
             Average polygon area: ",
            round(mean(sf$area,na.rm=TRUE),2),"
             Average polygon perimeter: ",
            round(mean(sf$perimeter,na.rm=TRUE),2))

  if(verbose && method=="HR"){
    area.cols=paste0("area_HR",vapply(HR.levels,
                                      level.tag,character(1)))
    means=vapply(sf[,area.cols,drop=FALSE],
                 mean,numeric(1),na.rm=TRUE)

    message("- Average home-range areas: ",
            paste(names(means),round(means,2),
                  sep="= ",collapse="; "))

    perimeter.cols=paste0("perimeter_HR",vapply(HR.levels,
                                      level.tag,character(1)))
    means=vapply(sf[,perimeter.cols,drop=FALSE],
                 mean,numeric(1),na.rm=TRUE)

    message("- Average home-range perimeters: ",
            paste(names(means),round(means,2),
                  sep="= ",collapse="; "))
  }

  return(as.data.frame(sf))
}
