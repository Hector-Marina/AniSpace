#' Estimate area utilisation
#'
#' @description
#' `areaUse` estimates individuals' area occupation periods and time budgets statistics.
#'
#' @param AniObj An AniSpace object containing the spatio-temporal information of the individuals.
#' @param Area Either `NULL`, a character vector containing area identifiers, or a numeric vector containing the indices of the areas to include (*Default: NULL; all areas*).
#' @param method A variable indicating the  selected analysis method (`InOut` or `TimeBudget`) (Default: *method="TimeBudget"*). `InOut` reports the entrance and exist time of each individual in each area included in the object. `TimeBudget` reports the individuals' time spent in each area.
#' @param t.lim A numeric variable indicating the time limit for considering an area occupied (*Default: 600*).
#' @param t.err A numeric variable indicating the time error than some positions could fall outside the area within the `t.lim` interval (*Default: 5% of t.lim*).
#' @param verbose A logical variable indicating whether the function will print relevant information (TRUE) or not (FALSE) (*Default: TRUE*).
#'
#' @return Descriptive statistics of the individual area usage periods and time budgets.
#'
#' @examples
#' # Estimate individual movement information
#' df.TB=areaUse(AniObj,method="TimeBudget")
#' head(df.TB)
#'
#' df.IO=areaUse(AniObj,method="InOut")
#' head(df.IO)
#'
#' @export

areaUse=function(AniObj, Area=NULL, method="TimeBudget", t.lim=600, t.err=t.lim*0.05, verbose=TRUE) {

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

  if (!is.numeric(t.lim) || length(t.lim) != 1L ||
      !is.finite(t.lim)  || t.lim <= 0)          stop("`t.lim` must be a single positive interger.")

  if (!is.numeric(t.err) || length(t.err) != 1L ||
      !is.finite(t.err)  || t.err < 0)           stop("`t.err` must be a single non-negative numeric value.")

  if(!is.logical(verbose))  stop("`verbose` is not logical")

  #--
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
      TB
    })
    TB=do.call(rbind, l)
    rownames(TB)=NULL
    return(TB)
  }

  #--
  #--
  if(method=="InOut"){
    if(verbose) message("Estimating the In-Out events from the areas...")

    l=lapply(seq_along(AniObj@Pos),function(ii){

      vt=AniObj@Pos[[ii]]$Time
      vx=AniObj@Pos[[ii]]$x
      vy=AniObj@Pos[[ii]]$y

      # Determine whether each position is inside each area
      o=lapply(NArea,function(i){
        P=AniObj@Area[[i]]$coords
        if(!all(P[1,]==P[nrow(P),])) P=rbind(P,P[1,])

        n=nrow(P)-1L
        IN=ED=logical(length(vx))
        eps=1e-9

        for(j in seq_len(n)){
          x1=P[j,1];   y1=P[j,2]
          x2=P[j+1,1]; y2=P[j+1,2]
          dx=x2-x1
          dy=y2-y1

          ED=ED | (
            abs((vx-x1)*dy-(vy-y1)*dx)<eps &
              vx>=pmin(x1,x2)-eps & vx<=pmax(x1,x2)+eps &
              vy>=pmin(y1,y2)-eps & vy<=pmax(y1,y2)+eps
          )

          denom=y2-y1
          crosses=((y1>vy)!=(y2>vy)) & denom!=0
          xint=dx*(vy-y1)/denom+x1

          IN=xor(IN,crosses & vx<xint)
        }

        IN | ED
      })

      # Create expanded temporal sequence
      max_step=(AniObj@TRes*2)-1

      if(length(vt)>1L){
        d=diff(vt)

        # Number of artificial positions inserted in each temporal gap.
        n.inter=integer(length(d))
        id=d>max_step

        n.inter[id]=as.integer(
          pmax(0,floor((d[id]-2)/2))
        )

        # Positions of the original observations in the expanded sequence
        obs.idx=c(
          1L,
          1L+cumsum(n.inter+1L)
        )

        ma.time=numeric(length(vt)+sum(n.inter))
        ma.time[obs.idx]=vt

        # Insert intermediate time points only for gaps requiring them
        big=which(n.inter>0L)

        if(length(big)>0L){
          z=base::sequence(n.inter[big])

          pseudo.idx=
            rep(obs.idx[big],n.inter[big])+z

          ma.time[pseudo.idx]=
            rep(vt[big],n.inter[big])+2*z
        }

      }else{
        obs.idx=1L
        ma.time=vt
      }

      # Detect In-Out events for each area
      mso=lapply(seq_along(o),function(i){

        # Original observations retain their In/Out status.
        # Artificial observations are considered outside.
        flag=logical(length(ma.time))
        flag[obs.idx]=o[[i]]

        # Identify changes between outside and inside
        if(length(flag)>1L){
          dr=which(flag[-length(flag)]!=flag[-1L])
        }else{
          dr=integer()
        }

        if(length(dr)>0L){

          # Transition information
          ds=data.frame(
            time=ma.time[dr+1L],
            from=as.integer(flag[dr]),
            to=as.integer(flag[dr+1L]),
            index=dr+1L
          )

          # Individual already inside area at beginning
          if(ds$from[1L]==1L && ds$to[1L]==0L){
            ds=rbind(
              data.frame(time=0,from=0L,to=1L,index=1L),
              ds
            )
          }

          # Individual remains inside area at final transition
          if(ds$from[nrow(ds)]==0L && ds$to[nrow(ds)]==1L){
            ds=rbind(
              ds,
              data.frame(
                time=max(ds$time),
                from=1L,
                to=0L,
                index=max(ds$index)
              )
            )
          }

          # Pair entrance and exit events
          i.in =seq(1L,nrow(ds),by=2L)
          i.out=seq(2L,nrow(ds),by=2L)

          ds1=data.frame(
            time_in =ds$time[i.in],
            time_out=ds$time[i.out],
            index_in =ds$index[i.in],
            index_out=ds$index[i.out]
          )

          # Merge occupation periods separated by less than t.err
          if(nrow(ds1)>1L){
            gap=c(
              Inf,
              ds1$time_in[-1L]-
                ds1$time_out[-nrow(ds1)]
            )
          }else{
            gap=Inf
          }

          grp=cumsum(gap>=t.err)

          first=!duplicated(grp)
          last =!duplicated(grp,fromLast=TRUE)

          ds2=data.frame(
            time_in =ds1$time_in[first],
            time_out=ds1$time_out[last],
            index_in =ds1$index_in[first],
            index_out=ds1$index_out[last]
          )

          # Occupation proportion using cumulative sums
          # instead of scanning the complete trajectory for every bout
          cs=c(0L,cumsum(flag))

          n.true=
            cs[ds2$index_out+1L]-
            cs[ds2$index_in]

          n.total=
            ds2$index_out-
            ds2$index_in+1L

          ds2$occupation=n.true/n.total

          # Convert relative to absolute time
          ds2$time_in =ds2$time_in +AniObj@TLim[1]
          ds2$time_out=ds2$time_out+AniObj@TLim[1]

          # Retain occupation periods longer than t.lim
          ds2=ds2[
            (ds2$time_out-ds2$time_in)>t.lim,
            ,
            drop=FALSE
          ]

          if(nrow(ds2)>0L){
            ds2=data.frame(
              NArea=NArea[i],
              Area=AniObj@Area[[NArea[i]]]$ID,
              time_in=ds2$time_in,
              time_out=ds2$time_out,
              occupation=ds2$occupation
            )
          }else{
            ds2=data.frame(
              NArea=NArea[i],
              Area=AniObj@Area[[NArea[i]]]$ID,
              time_in=0,
              time_out=0,
              occupation=0
            )
          }

        }else{
          ds2=data.frame(
            NArea=NArea[i],
            Area=AniObj@Area[[NArea[i]]]$ID,
            time_in=0,
            time_out=0,
            occupation=0
          )
        }

        return(ds2)
      })

      IO=cbind(
        NIDs=AniObj@NIDs[ii],
        IDs=AniObj@IDs[ii],
        do.call(rbind,mso)
      )

      return(IO)
    })

    IOL=do.call(rbind,l)
    rownames(IOL)=NULL

    # Format temporal information
    IOL$time_in[IOL$time_in==0]=NA_real_
    IOL$time_out[IOL$time_out==0]=NA_real_

    IOL$time_in=as.POSIXct(IOL$time_in,origin="1970-01-01",tz="UTC")
    IOL$time_out=as.POSIXct(IOL$time_out,origin="1970-01-01",tz="UTC")

    # Estimate bouts duration
    IOL$duration=as.numeric(difftime(IOL$time_out,IOL$time_in,units="secs"))

    return(IOL)
  }
}
