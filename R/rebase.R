#' Rebase AniSpace object
#'
#' @description
#' `rebase()` Rebase AniSpace object.
#'
#' @param AniObj An AniSpace object
#'
#' @examples
#' df=rebase(df)
#'
#' @export
#'
rebase=function(AniObj){
  # Rebase NIDs
  if(min(AniObj@NIDs)>1){
    AniObj@NIDs=AniObj@NIDs-min(AniObj@NIDs)+1
  }

  # Rebase Time
  vt=unlist(lapply(AniObj@Pos, function(p) p$Time), use.names = FALSE)
  if(min(AniObj@NIDs)>0){
    AniObj@TLim[1]=AniObj@TLim[1]+min(vt)
    AniObj@TLim[2]=AniObj@TLim[1]+max(vt)-min(vt)

    l=lapply(seq_along(AniObj@Pos), function(ii) {
      return(list(Time=AniObj@Pos[[ii]]$Time-min(vt),x=AniObj@Pos[[ii]]$x,y=AniObj@Pos[[ii]]$y))
    })
    names(l)=names(AniObj@Pos)
    AniObj@Pos=l

  }
  return(AniObj)

}
