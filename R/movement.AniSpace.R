#' Analyse individual movement information
#'
#' @description
#' `movement.AniSpace` estimates the distance walked, speed and the turning angles per individual, and performs a cluster analysis of the areas included in the `AniObj` object to classify different behaviours.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param k.means A logical variable indicating whether the Kmeans cluster analysis will be executed (TRUE) or not (FALSE) (*Default: FALSE*). `k.means` will be automatically set to TRUE if `k.mov` value is provided. The Kmeans cluster analysis uses the speed and turning angle information from all individuals in the `AniObj` to assign `k.mov` behaviours to the different areas detailed in the object.
#' @param k.mov  A numeric variable indicating the number of behaviours used to estimate similarity between areas applying a Kmeans cluster analysis (*Default: NULL*).
#' @param k.max  A numeric variable indicating the maximum number of clusters tested on the optimisation of the K-means cluster analysis (*Default: 10*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords individual movement information
#'
#' @return Descriptive information about how individuals move and behave in different areas.
#'
#' @examples
#' # Estimate individual movement information
#' df.mov=movement.AniSpace(df)
#' head(df.mov)
#'
#' # Group different areas based on movement information fixing number of behaviour to 2 (k.mov=2)
#' df.mov2=movement.AniSpace(df,k.mov=2)
#' head(df.mov2[[2]])
#'
#' @import dplyr
#' @export

movement.AniSpace=function(AniObj, k.means=FALSE, k.mov=NULL, k.max=10, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if(!is.logical(k.means)) stop("`k.means` must be logical")
  if(!is.null(k.mov)){
    if(!is.numeric(k.mov))  stop("`k.mov` is not numeric")
  }

  if(!is.numeric(k.max))  stop("`k.max` is not numeric")
  if(!is.logical(verbose)) stop("`verbose` must be logical")


  # Estimate distance, speed and turning angles
  if(verbose) message("Estimating distance, speed and turning angles data...")
  l=lapply(seq_along(AniObj@Pos), function(ii) {

    p=as.data.frame(AniObj@Pos[[ii]])

    m=suppressWarnings(p %>%
      dplyr::mutate(Time_interval = (Time-dplyr::lag(Time)),
                    walked_dist = sqrt((dplyr::lag(x)-x)^2 + (dplyr::lag(y)-y)^2),
                    speed= sqrt((dplyr::lag(x)-x)^2 + (dplyr::lag(y)-y)^2)/(Time-dplyr::lag(Time)),
                    Turn_acos=acos(
                      ((sqrt((dplyr::lag(x) - dplyr::lag(dplyr::lag(x)))^2 + (dplyr::lag(y) - dplyr::lag(dplyr::lag(y)))^2))^2    + # P21
                         (sqrt((dplyr::lag(x)             - x            )^2 + (dplyr::lag(y)             - y            )^2))^2  - #P23
                         (sqrt((dplyr::lag(dplyr::lag(x)) - x            )^2 + (dplyr::lag(dplyr::lag(y)) - y            )^2))^2) / #P13
                        (2 *
                           (sqrt((dplyr::lag(x) - dplyr::lag(dplyr::lag(x)))^2 + (dplyr::lag(y) - dplyr::lag(dplyr::lag(y)))^2))   *
                           (sqrt((dplyr::lag(x)             - x            )^2 + (dplyr::lag(y)             - y            )^2)))  ), #(2 * P21 * P23))
                    Turn_direction=sign((dplyr::lag(x) - dplyr::lag(dplyr::lag(x))) * (y - dplyr::lag(dplyr::lag(y))) - (dplyr::lag(y) - dplyr::lag(dplyr::lag(y))) * (x - dplyr::lag(dplyr::lag(x)))),
                    Turn_angle=if_else(Turn_direction < 0, 360 - Turn_acos * 180/pi, Turn_acos * 180/pi)
      ))
    # Flip Turn_angle
    m$Turn_angle=ifelse(m$Turn_angle>180, 360 - m$Turn_angle + 180, 180 - m$Turn_angle)

    m=data.frame(NIDs=AniObj@NIDs[ii],IDs=AniObj@IDs[ii],m)

    # Filter non-numeric values
    m=m[,-which(colnames(m)%in%c("Turn_acos","Turn_direction"))]
    m=as.data.frame(m[complete.cases(m),])
    m=m[is.finite(m$speed),]

    if(verbose) sprog(ii,length(AniObj@NIDs))
    return(m)

  })
  m=do.call(rbind, l)


  if(!is.null(k.mov)) k.means=TRUE
  if(k.means){
    # Estimate optimal number of k.mov
    if(is.null(k.mov)){
      if(verbose) message("\n Estimating optimal number of areas by comparing the within-cluster sum of squares (WCSS) against the number of clusters...")
      # Calculate WCSS for each k
      wcss=sapply(1:k.max, function(k) {
        kmeans(m[sample(nrow(m),ceiling(nrow(m)*0.10)),c("speed","Turn_angle")], centers = k)$tot.withinss
      })

      # Estimate k.mov based on descrease rate
      wcssd=as.matrix(dist(wcss))[,1]/max(wcss)
      k.mov=as.numeric(which( (wcssd-c(wcssd[2:length(wcssd)],max(wcssd))) > (-0.05))[1])

      if(verbose) {
        p=plot(1:k.max, wcss, type = "b", pch = 19, frame = FALSE,
            xlab = "Number of clusters K",
            ylab = "Total within-clusters sum of squares")
        p=p+points(k.mov,wcss[k.mov],col="red",pch = 16)
        print(p)
        message(paste("k.mov =",k.mov))
      }
    }

    # k-means cluster analysis
    mov.k=kmeans(m[,c("speed","Turn_angle")], centers=k.mov, iter.max = 10)

    # Extract basic stats about the clusters
    mov.stats=stats::aggregate(m[,c("speed","Turn_angle")],by=list(cluster=mov.k$cluster), mean)

    # Assign to each point a cluster
    m=cbind(m, cluster = mov.k$cluster)

    # Proportion of clusters per area
    if(length(AniObj@Area)>0){
      if(verbose) message("Assigning clusters to the defined areas...")

      ca=as.data.frame(matrix(0,nrow=length(AniObj@Area), ncol=(k.mov+2)))

      vx=m$x
      vy=m$y

      In_mat=sapply(seq_along(AniObj@Area), function(jj) {
        P=AniObj@Area[[jj]]$coords

        IN=ED=logical(length(vx))

        for (j in 1:(nrow(P) - 1L)) {
          x1=P[j, 1]; y1=P[j, 2]
          x2=P[j+1, 1]; y2=P[j+1, 2]
          dx=x2 - x1; dy=y2 - y1
          seglen2=dx*dx + dy*dy

          denom=y2 - y1
          crosses=((y1 > vy) != (y2 > vy)) & (denom != 0)
          xint=(dx * (vy - y1)) / denom + x1
          IN=xor(IN, crosses & (vx < xint))

          if (seglen2 > 0) {
            t=((vx - x1) * dx + (vy - y1) * dy) / seglen2
            t=pmin(1, pmax(0, t))            # clamp to segment
            cx=x1 + t * dx; cy=y1 + t * dy
            dist2=(vx - cx)^2 + (vy - cy)^2
            ED=ED | (dist2 <= 0^2)
          } else {
            ED=ED | ((vx - x1)^2 + (vy - y1)^2 <= 0^2)
          }
        }
        IN | ED
      }, simplify = "matrix")


      for (ii in c(1:ncol(In_mat))){
        ca[ii,]=c(AniObj@Area[[ii]]$ID, table(c(m[In_mat[,ii],"cluster"],1:k.mov))-1,sum(In_mat[,ii]))
      }

      colnames(ca)=c("ID",paste0("clust_",1:k.mov),"Total")
    }
  }

  if(k.means && length(AniObj@Area)>0){
    movl=list()
    movl[["movement"]]=as.data.frame(m)
    movl[["areas"]]=ca
    return(movl)
  }else{
    return(m)
  }
}
