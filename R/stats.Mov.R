#' Analyse individual movement and area-level behavioural clustering
#'
#' @description
#' `stats.Mov` calculates step distances, speeds, and turning angles for each individual. When requested, it applies k-means clustering to movement observations and summarizes the occurrence of each movement-behaviour cluster within the areas defined in `AniObj`.
#'
#' @param AniObj An object of class `AniSpace` containing the individuals' spatio-temporal position information.
#' @param k.means Logical. If `TRUE`, a k-means cluster analysis is performed using step distance, speed, and turning angle (*Default: FALSE*). The Kmeans cluster analysis uses the step distance, speed and turning angle information from all individuals in the `AniObj` object to assign `k.mov` behaviours to the different areas detailed in the object.
#' @param k.mov  A positive integer specifying the number of movement-behaviour clusters. If `NULL`, the number of clusters is estimated by comparing the within-cluster sum of squares for values from 1 to `k.max` (*Default: NULL*).
#' @param k.max  A positive integer specifying the maximum number of clusters considered when estimating `k.mov` (*Default: 10*).
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#'
#' @keywords individual movement information
#'
#' @return A list containing:
#' \itemize{
#' ** \item `movement`: a data frame with position coordinates, time intervals, step distances, speeds, turning angles, and cluster assignments when clustering is performed.**
#' ** \item `cluster`: a data frame summarizing cluster counts within each defined area, or NULL when clustering is not performed or no areas are defined.**
#' }
#'
#' @examples
#' # Calculate individual movement information
#' df.mov=stats.Mov(AniObj,verbose=FALSE)
#' head(df.mov$movement)
#'
#' # Classify movement into two behavioural clusters
#' set.seed(123)
#' df.mov2=stats.Mov(AniObj,k.means=TRUE,k.mov=2,verbose=FALSE)
#'
#' # Inspect movement information and assigned clusters
#' head(df.mov2$movement)
#'
#' # Inspect the occurrence of each cluster within the defined areas
#' head(df.mov2$cluster)
#'
#' @export

stats.Mov=function(AniObj, k.means=FALSE, k.mov=NULL, k.max=10, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if(!is.logical(k.means)) stop("`k.means` must be logical")
  if(!is.null(k.mov)){
    if(!is.numeric(k.mov))  stop("`k.mov` is not numeric")
  }

  if(!is.numeric(k.max))  stop("`k.max` is not numeric")
  if(!is.logical(verbose)) stop("`verbose` must be logical")


  # Step distance, speed and turning angles
  if(verbose) message("Estimating distance, speed and turning angles data...")
  l=lapply(seq_along(AniObj@Pos), function(i) {
    p=AniObj@Pos[[i]]

    n=length(p$Time)
    if (n < 3L) return(NA)

    dt=diff(p$Time)
    dx=diff(p$x)
    dy=diff(p$y)

    # step distance and speed
    speed   = sqrt(dx^2 + dy^2) / dt
    speed[!is.finite(speed) | dt <= 0] <- NA_real_

    # turning angles
    turn_acos     =rep(NA_real_, n)
    turn_direction=rep(NA_real_, n)
    turn_angle    =rep(NA_real_, n)

    dx1=dx[-length(dx)]
    dy1=dy[-length(dy)]

    dx2=dx[-1L]
    dy2=dy[-1L]

    P21=sqrt(dx1^2 + dy1^2)
    P23=sqrt(dx2^2 + dy2^2)
    P13=sqrt((p$x[3:n] - p$x[1:(n - 2L)])^2 + (p$y[3:n] - p$y[1:(n - 2L)])^2)

    cos_angle=(P21^2 + P23^2 - P13^2) / (2 * P21 * P23)
    cos_angle=pmax(-1, pmin(1, cos_angle))

    ok=is.finite(cos_angle) & P21 > 0 & P23 > 0

    angle_rad=rep(NA_real_, n - 2L)
    angle_rad[ok]=acos(cos_angle[ok])

    direction=sign(dx1 * dy2 - dy1 * dx2)

    angle_deg=ifelse(direction < 0, 360 - angle_rad * 180 / pi, angle_rad * 180 / pi)

    # turn_acos[3:n]     =angle_rad
    # turn_direction[3:n]=direction
    turn_angle[3:n]    =angle_deg

    turn_angle=ifelse(turn_angle > 180, 540 - turn_angle, 180 - turn_angle)


    return(data.frame(NIDs=AniObj@NIDs[i],IDs=AniObj@IDs[i],x=p$x,y=p$y,
               time_interval=c(NA_real_,dt),dist=c(NA_real_,sqrt(dx^2 + dy^2)),speed=c(NA_real_,speed),
               #turn_acos=turn_acos,turn_direction=turn_direction,
               turn_angle=turn_angle))
  })
  l=do.call(rbind, l)

  # Kmeans cluster analysis
  if(k.means){
    m=l[stats::complete.cases(l),]

    # Estimate optimal number of k.mov
    if(is.null(k.mov)){
      if(verbose) message("Estimating optimal number of areas by comparing the within-cluster sum of squares (WCSS) against the number of clusters...")
      # Calculate WCSS for each k
      wcss=sapply(1:k.max, function(k) {
        stats::kmeans(m[sample(nrow(m),ceiling(nrow(m)*0.10)),c("dist","speed","turn_angle")], centers = k)$tot.withinss
      })

      # Estimate k.mov based on descrease rate
      wcssd=as.matrix(stats::dist(wcss))[,1]/max(wcss)
      k.mov=as.numeric(which( (wcssd-c(wcssd[2:length(wcssd)],max(wcssd))) > (-0.05))[1])

      if(verbose) {
        graphics::plot(1:k.max, wcss, type = "b", pch = 19,  frame.plot = FALSE,
            xlab = "Number of movement-behaviour clusters (K.mov)",
            ylab = "Total within-cluster sum of squares",
            main = "Number of Movement-Behaviour Clusters")
        graphics::points(k.mov,wcss[k.mov],col="red",pch = 16)
        message(paste("Selected number of movement-behaviour clusters: `k.mov` = ", k.mov,"."))
      }
    }

    # k-means cluster analysis
    mov.k=stats::kmeans(m[,c("dist","speed","turn_angle")], centers=k.mov, iter.max = 10)

    # Extract basic stats about the clusters
    mov.stats=stats::aggregate(m[,c("dist","speed","turn_angle")],by=list(cluster=mov.k$cluster), mean)

    # Assign to each point a cluster
    m=cbind(m, cluster = mov.k$cluster)
    ok=stats::complete.cases(l)
    l$cluster=NA_integer_
    l$cluster[ok]=mov.k$cluster

    # Proportion of clusters per area
    if(length(AniObj@Area)>0){
      if(verbose) message("Assigning clusters to the defined areas...")

      ca=data.frame(ID = character(length(AniObj@Area)),
        matrix(0,nrow = length(AniObj@Area),ncol = k.mov + 1L),
        check.names = FALSE)

      names(ca)=c("ID",paste0("clust_", seq_len(k.mov)),"Total")

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
        ca$ID[ii]=AniObj@Area[[ii]]$ID
        ca[ii, 2:(k.mov + 1L)]=as.numeric(table(c(m[In_mat[,ii],"cluster"],1:k.mov)) - 1L)
        ca$Total[ii]=sum(In_mat[, ii], na.rm = TRUE)
      }
    }
  }

  # Return list
  movl=list(movement = l,
            cluster = if (exists("ca", inherits = FALSE)) ca else NULL )
  return(movl)
}
