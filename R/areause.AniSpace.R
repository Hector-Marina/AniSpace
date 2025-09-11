#' Estimate area utilisation
#'
#' @description
#' `areause.AniSpace` estimates individuals' area occupation periods and time budgets statistics.
#'
#' @param AniObj AAn AniSpace object containing the spatio-temporal information of the individuals.
#' @param method A variable indicating the  selected analysis method (`InOut` or `TimeBudget`) (Default: *method="TimeBudget"*). `InOut` reports the entrance and exist time of each individual in each area included in the object. `TimeBudget` reports the individuals' time spent in each area.
#' @param t.lim A numeric variable indicating the time limit for considering an area occupied (*Default: 600*).
#' @param t.err A numeric variable indicating the time error than some positions could fall outside the area within the `t.lim` interval (*Default: 5% of t.lim*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @keywords individual area usage time budget information
#'
#' @return Descriptive statistics of the individual area usage periods and time budgets.
#'
#' @examples
#' # Estimate individual movement information
#' df.TB=areause.AniSpace(df,method="TimeBudget")
#' head(df.TB)
#'
#' df.IO=areause.AniSpace(df,method="InOut")
#' head(df.IO)
#'
#' @export

areause.AniSpace=function(AniObj, Area=NULL, method="TimeBudget", t.lim=600, t.err=t.lim*0.05, verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if(is.null(Area)) {
    NArea=1:length(AniObj@Area)
  } else {
    if(is.character(Area)){
      l=sapply(seq_along(AniObj@Area), function(ii) {AniObj@Area[[ii]]$ID})
      if(!any(Area%in%l))          stop("`Area` not found in AniSpace object")
      NArea=which(l%in%Area)
    }else if (is.numeric(Area)){
      NArea=as.numeric(Area)
    } else { stop("`Area` must be class character or numeric") }
  }
  NmArea=sapply(NArea, function(ii) {AniObj@Area[[ii]]$ID})

  if (!method %in% c("InOut", "TimeBudget")) {
    stop("Invalid `method`: must be one of 'InOut' or 'TimeBudget'.")
  }

  if(!is.numeric(t.lim))  stop("`t.lim` is not numeric")
  if(!is.numeric(t.err))  stop("`t.err` is not numeric")

  if(!is.logical(verbose))  stop("`verbose` is not llogical")

  if (method=="TimeBudget"){
    if(verbose) message("Estimating the time budget in the areas...")

    l=lapply(seq_along(AniObj@Pos), function(ii) {

      vx=AniObj@Pos[[ii]]$x
      vy=AniObj@Pos[[ii]]$y

      o = sapply(NArea, function(i) {
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

      TB=data.frame(NIDs=AniObj@NIDs[ii], IDs=AniObj@IDs[ii],
                    Area=NmArea, occupation=o,
                    npositions=length(vx), expected_pos=diff(AniObj@TLim)/AniObj@TRes)

      if(verbose) sprog(ii,length(AniObj@NIDs))
      TB
    })
    TB=do.call(rbind, l)
    rownames(TB)=NULL
    return(TB)
  }

  if (method=="InOut"){
    if(verbose) message("Estimating the In-Out events from the areas...")

    l=lapply(seq_along(AniObj@Pos), function(ii) {

      vt=AniObj@Pos[[ii]]$Time
      vx=AniObj@Pos[[ii]]$x
      vy=AniObj@Pos[[ii]]$y

      o = lapply(NArea, function(i) {
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
        IN | ED
      })

      max_step=(AniObj@TRes*2)-1
      mso = lapply(seq(length(o)), function(i) {
        ms=mapply(function(a, b, fb) {
          d=b-a
          if (d <= max_step) {
            data.frame(time = b, flag = as.logical(fb))
          } else {
            inc=if (d %% 2L == 0L) rep(2L, d/2L) else c(rep(2L, (d - 3L)/2L), 3L)
            inter=a + cumsum(inc); inter=inter[inter < b]
            data.frame(time = c(inter, b),
                       flag = c(rep(FALSE, length(inter)), as.logical(fb)))
          }
        }, vt[-length(vt)], vt[-1], o[[i]][-1], SIMPLIFY = FALSE)

        ma=rbind(data.frame(time=vt[1],flag=o[[i]][1]),do.call(rbind, ms))

        dr=which(ma$flag[-length(ma$flag)]!=ma$flag[-1])
        ds=cbind(ma$time[-1],ma$flag[-length(ma$flag)],ma$flag[-1])[dr,]
        if (nrow(ds)>0) {
          if(ds[1,2]==1        & ds[1,3]==0       ) {ds=rbind(cbind(0,0,1),ds)}
          if(ds[nrow(ds),2]==0 & ds[nrow(ds),3]==1) {ds=rbind(ds,cbind(max(ds[,1]),1,0))}

          ds1=cbind(ds[seq(1, nrow(ds), by = 2), 1], ds[seq(2, nrow(ds), by = 2), 1])

          gap=c(Inf, ds1[-1, 1] - ds1[-nrow(ds1), 2])
          grp=cumsum(gap >= t.err)

          sd =split(seq_len(nrow(ds1)), grp)
          ds2=do.call(rbind, lapply(sd, function(ix) {
            c(ds1[ix[1], 1], ds1[ix[length(ix)], 2])     # first start, last end
          }))
          colnames(ds2)=colnames(ds1)
          rownames(ds2)=NULL
          ds2=as.data.frame(ds2)

          ds2$occ=sapply(seq(nrow(ds2)), function(iii) {
            sum(ma$flag[ds2[iii,1]<=ma$time & ma$time<=ds2[iii,2] ])/sum(ds2[iii,1]<=ma$time & ma$time<=ds2[iii,2])
          })

          ds2[,1]=ds2[,1]+AniObj@TLim[1]
          ds2[,2]=ds2[,2]+AniObj@TLim[1]

          ds2=ds2[(ds2[,2]-ds2[,1])>t.lim,, drop = FALSE]
          if(nrow(ds2)>0){
            ds2=cbind(NArea[i],AniObj@Area[[NArea[i]]]$ID,ds2)
          }else{
            ds2=data.frame(NArea[i],AniObj@Area[[NArea[i]]]$ID,0,0,0)
          }

        }else{
          ds2=data.frame(NArea[i],AniObj@Area[[NArea[i]]]$ID,0,0,0)
        }
        colnames(ds2)=c("NArea","Area","time_in","time_out","occupation")
        ds2
      })

      IO=cbind(NIDs=AniObj@NIDs[ii],IDs=AniObj@IDs[ii],do.call(rbind, mso))
      if(verbose) sprog(ii,length(AniObj@NIDs))
      return(IO)
    })

    IOL=do.call(rbind, l)
    rownames(IOL)=NULL
    return(IOL)
  }
}
