#' Estimate pairwise utilisation-distribution similarity
#'
#' @description
#' `UDsim` estimates pairwise spatial-utilisation similarities between
#' individuals in an `AniSpace` object. The calculation depends on the
#' utilisation-distribution method previously applied with `areaUD`:
#' grid-cell use, polygon overlap, or kernel home-range overlap. Similarities
#' range from 0, indicating no shared utilisation, to 1, indicating complete
#' similarity of the focal individual's utilisation.
#'
#' @param AniObj An object of class `AniSpace` containing utilisation
#' distributions previously estimated with `areaUD`.
#' @param sim.method A character value specifying whether similarity is based
#' on spatial occupancy (`"binary"`) or the magnitude of utilisation
#' (`"continuous"`). The calculation depends on the stored utilisation method:
#'
#' * For `"grid"`, `"binary"` estimates the proportion of the focal
#'   individual's occupied cells also occupied by the other individual.
#'   `"continuous"` estimates the proportion of the focal individual's
#'   locations shared with the other individual.
#' * For `"poly"`, `"binary"` estimates the proportion of the focal polygon's
#'   grid cells also contained in the other polygon. `"continuous"` estimates
#'   the proportion of the focal polygon's exact area overlapped by the other
#'   polygon.
#' * For `"HR"`, `"binary"` estimates the proportion of the focal individual's
#'   cells within the `HR.level` contour also contained within the other
#'   individual's contour (Hurlbert's statistic). `"continuous"` estimates the
#'   conditional Bhattacharyya affinity between both utilisation distributions
#'   after restricting and normalising them within their `HR.level` contours.
#'
#' Grid, polygon, and binary home-range similarities are directional, whereas
#' continuous home-range similarity is symmetric (*Default: "continuous"*).
#' @param HR.level A numeric value between 0 and 100 specifying the probability
#' contour used for binary and continuous kernel home-range comparisons. This
#' argument is only used when the stored utilisation method is `"HR"`
#' (*Default: 95*).
#' @param verbose A logical value indicating whether progress messages are
#' printed (*Default: TRUE*).
#'
#' @return The modified `AniSpace` object with the pairwise similarity matrix
#' stored in the `UDsim` slot. For directional methods, element `[i,j]`
#' represents the proportion of individual `i`'s utilisation shared with
#' individual `j`.
#'
#' @examples
#' # Filter individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=c(3:7))
#'
#' # Estimate pairwise grid-based utilisation-distribution similarities
#' df.UD=areaUD(df.ID.filt,method="grid")
#' df.UD.sim=UDsim(df.UD)
#'
#' # Estimate pairwise polygon-based utilisation-distribution similarities
#' df.poly=areaUD(df.ID.filt,method="poly")
#' df.poly.sim=UDsim(df.poly)
#'
#' # Estimate pairwise kernel-based utilisation-distribution similarities
#' df.HR=areaUD(df.ID.filt,method="HR")
#' df.HR.sim=UDsim(df.HR)
#'
#' @export
#'
#'

UDsim=function(AniObj, sim.method="continuous", HR.level=95, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (length(AniObj@UD) == 0L) stop("No utilisation-distribution results were found in `AniObj`.", call. = FALSE)

  if (!is.character(sim.method) || length(sim.method) != 1L || is.na(sim.method) ||
      !sim.method %in% c("binary","continuous")) {
    stop("Invalid `sim.method`: must be one of 'binary' or 'continuous'.")
  }

  if (!is.numeric(HR.level) || length(HR.level) != 1L ||
      !is.finite(HR.level) || HR.level <= 0 || HR.level >= 100) {
    stop("`HR.level` must be a single numeric value between 0 and 100.",call. = FALSE)}

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

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

  # Individuals with UD information
  n=length(AniObj@IDs)
  M=matrix(NA_real_,nrow=n,ncol=n,dimnames=list(AniObj@IDs,AniObj@IDs))
  valid=vapply(UD,function(x) !is.null(x),logical(1))
  diag(M)=ifelse(valid,1,NA_real_)
  if(sum(valid)<2L) stop("The number of individuals with `UD` information needs to be larger than one.")

  #----------------------------------------------------------
  # Grid-based utilisation distribution
  #----------------------------------------------------------
  if (method == "grid") {
    if(verbose) message("- Estimating pairwise grid-based utilisation similarities.")

    n=nrow(M)
    # Pairwise proportion of cells visited out of the individual visited//
    # Pairwise proportion of locations in each grid cell
    for(i in seq_len(n-1L)){
      if(!valid[i]) next
      for(j in (i+1L):n){
        if(!valid[j]) next

        d=merge(UD[[i]],UD[[j]],by="cell",all=TRUE,suffixes=c(".i",".j"))
        d$z.i[is.na(d$z.i)]=0
        d$z.j[is.na(d$z.j)]=0

        common=d$z.i>0 & d$z.j>0
        if(sum(common)==0){
          M[i,j]=M[j,i]=0
          next
        }

        if(sim.method=="binary"){
          M[i,j]=sum(common)/sum(d$z.i>0)
          M[j,i]=sum(common)/sum(d$z.j>0)
        }
        if(sim.method=="continuous"){
          row.min=pmin(d$z.i[common],d$z.j[common])
          M[i,j]= sum(row.min)/sum(d$z.i)
          M[j,i]= sum(row.min)/sum(d$z.j)
        }
      }
    }
  }

  #----------------------------------------------------------
  # Polygon-based utilisation distribution
  #----------------------------------------------------------
  if(method=="poly"){
    if(verbose) message("- Estimating pairwise polygon-based utilisation similarities.")

    if(sim.method=="binary"){
      xbreaks=AniObj@UD$parameters$xbreaks
      ybreaks=AniObj@UD$parameters$ybreaks

      G=expand.grid(
        x=utils::head(xbreaks,-1L)+diff(xbreaks)/2,
        y=utils::head(ybreaks,-1L)+diff(ybreaks)/2)

      cells=lapply(UD,function(p){
        if(is.null(p)) return(NA_real_)
        which(sp::point.in.polygon(G$x,G$y,p$x,p$y)>0)
      })
    }

    if(sim.method=="continuous"){
      area=vapply(UD,function(p){
        if(is.null(p)) return(NA_real_)
        0.5*abs(sum(p$x[-length(p$x)]*p$y[-1L]-p$x[-1L]*p$y[-length(p$y)]))
      },numeric(1))
    }

    for(i in seq_len(n-1L)){
      if(!valid[i]) next
      for(j in (i+1L):n){
        if(!valid[j]) next

        if(sim.method=="binary"){
          common=length(intersect(cells[[i]],cells[[j]]))
          if(sum(common)==0){M[i,j]=M[j,i]=0; next}

          M[i,j]=common/length(cells[[i]])
          M[j,i]=common/length(cells[[j]])
        }

        if(sim.method=="continuous"){
          overlap=polyclip::polyclip(
            list(x=UD[[i]]$x,y=UD[[i]]$y),
            list(x=UD[[j]]$x,y=UD[[j]]$y),
            op="intersection")

          shared=if(length(overlap)==0L){0} else{
            sum(vapply(overlap,function(p) {
              x=c(p$x, p$x[1L])
              y=c(p$y, p$y[1L])
              0.5*abs(sum(x[-length(x)]*y[-1L]-x[-1L]*y[-length(y)]))}
              ,numeric(1)))}

          shared=min(shared,area[i],area[j])
          M[i,j]=shared/area[i]
          M[j,i]=shared/area[j]
        }
      }
    }
  }


  #----------------------------------------------------------
  # Kernel home range
  #----------------------------------------------------------
  if (method == "HR") {
    if(verbose) message("- Estimating pairwise kernel home-range similarities.")

    K=lapply(UD,function(x){
      if(is.null(x)){ return(NULL)}
      d=as.data.frame(x)
      v=as.data.frame(adehabitatHR::getvolumeUD(x))
      colnames(d)=colnames(v)=c("ud","x","y")
      d=d[order(d$x,d$y),]
      v=v[order(v$x,v$y),]
      list(z=d$ud,vol=v$ud)
    })

    cell.area=prod(sp::gridparameters(UD[[which(valid)[1L]]])[,2])

    if(sim.method=="binary"){
      cells=lapply(K,function(x){
        if(is.null(x)) return(integer())
        which(is.finite(x$vol) & x$vol<=HR.level)
      })
    }

    if(sim.method=="continuous"){
      P=lapply(K,function(x){
        if(is.null(x)) return(NULL)
        z=x$z
        z[!is.finite(z) | !is.finite(x$vol) | x$vol>HR.level]=0
        mass=sum(z)*cell.area
        if(!is.finite(mass) || mass<=0) return(NULL)
        z#/mass
      })
    }


    for(i in seq_len(n-1L)){
      if(!valid[i]) next
      for(j in (i+1L):n){
        if(!valid[j]) next

        if(sim.method=="binary"){
          if(length(cells[[i]])==0L || length(cells[[j]])==0L){
            M[i,j]=M[j,i]=NA_real_
            next
          }

          common=length(intersect(cells[[i]],cells[[j]]))
          M[i,j]=common/length(cells[[i]])
          M[j,i]=common/length(cells[[j]])
        }

        if(sim.method=="continuous"){
          if(is.null(P[[i]]) || is.null(P[[j]])){
            M[i,j]=M[j,i]=NA_real_
            next
          }

          BA=sum(sqrt(P[[i]]*P[[j]]))*cell.area
          M[i,j]=M[j,i]=max(0,min(1,BA))
        }
      }
    }
  }

  # Add matrix to AniObj
  AniObj@UDsim=list(method=method, sim.method=sim.method, HR.level=HR.level, M=M)

  # Validate AniObj
  VAL=validate(AniObj)
  if(!VAL) stop("The spatial-interaction results produced an invalid `AniObj` object.")

  return(AniObj)
}
