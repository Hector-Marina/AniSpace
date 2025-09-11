#' 1) Show method for AniSpace
#'
#' @param AniObj An AniSpace object
#'
#' @importFrom methods setMethod show
#'
#' @export
#'
setMethod("show", signature = "AniSpace", function(object) {
  cat("-------AniSpace: Spatio-temporal object-------\n")

  # Individuals
  if (length(object@NIDs) > 5){
    cat("# Individuals:", length(object@NIDs),"(", paste(head(object@IDs,5), collapse = ", "), ",...)", "\n")
  } else {
    cat("# Individuals:", length(object@NIDs),"(", paste(object@IDs, collapse = ", "),")", "\n")
  }

  # Information
  if (length(object@Info)>0){
    if (length(object@Info) > 5){
      cat("# Traits:", length(object@Info),"(", paste(head(names(object@Info),5), collapse = ", "), ",...)", "\n")
    } else {
      cat("# Traits:", length(object@Info),"(", paste(names(object@Info), collapse = ", "),")", "\n")
    }
  }

  # Areas
  if (length(object@Area)>0){
    n=sapply(object@Area, `[[`, "ID") |> as.character()
    if (length(object@Area) > 5){
      cat("# Areas:", length(object@Area),"(", paste(head(n,5), collapse = ", "), ",...)", "\n")
    } else {
      cat("# Areas:", length(object@Area),"(", paste(n, collapse = ", "),")", "\n")
    }
  }

  # Positions
  x=sapply(object@Pos, function(p) length(p$x))
  cat("# Positions per ID: mean=",round(mean(x),2),"; sd=",round(sd(x),2), "\n")
  cat("% missing positions (Time resolution=",epoch2time(object@TRes),"):",
      round((1-length(unlist(lapply(object@Pos, function(p) p$Time), use.names = FALSE))/
               ((object@TLim[2]-object@TLim[1])*length(object@NIDs)/object@TRes)) *100 ,2),"%\n")

  # Bounds
  if (length(object@Area)>0){
    coords=do.call(rbind, lapply(seq_along(object@Area), function(ii) object@Area[[ii]]$coords[, c("x","y"), drop = FALSE]))
    xlim=range(coords[, "x"], na.rm = TRUE)
    ylim=range(coords[, "y"], na.rm = TRUE)

    cat("  Position bounds: x=c(", round(xlim[1],2),",",round(xlim[2],2),"); y=c(", round(ylim[1],2),",",round(ylim[2],2), ")\n")
  }

  # Time span
  cat("  Time span:", object@TLim[1],"(",as.character(as.POSIXct(object@TLim[1], origin = "1970-01-01", tz="UTC")),") -",
                      object@TLim[2],"(",as.character(as.POSIXct(object@TLim[2], origin = "1970-01-01", tz="UTC")),") \n")
  cat("  Duration:", epoch2time(object@TLim[2]-object@TLim[1]), "\n")

})


# 2) names method for AniSpace: make names(AniSpace) return the slotNames
#'
#' @export
setMethod("names", signature = "AniSpace", function(x) slotNames(x))



# 3) merge method for AniSpace
#'
#' @export
setMethod("merge", signature = c(x = "AniSpace", y = "AniSpace"), function(x, y, verbose=TRUE,...){

  if (!inherits(x, "AniSpace")) stop("`x` must be class 'AniSpace'.")
  if( !validate(x))             stop("Invalid `x` object.")
  if (!inherits(y, "AniSpace")) stop("`y` must be class 'AniSpace'.")
  if( !validate(y))             stop("Invalid `y` object.")
  if(!is.logical(verbose)) stop("`verbose` must be logical")


  ddS=FALSE # Duplicate switches

  # Create the AniSpace object
  xx=new("AniSpace",
          NIDs = as.numeric(),
          IDs  = as.character(),
          Info = list(),
          TLim = as.numeric(),
          TRes = as.numeric(),
          Pos  = list(),
          Area = list())

  # IDs & NIDs
  IDs1=setdiff(  x@IDs, y@IDs)
  IDs2=setdiff(  y@IDs, x@IDs)
  IDs3=intersect(x@IDs, y@IDs)

  xx@IDs =unique(c(x@IDs, y@IDs))
  xx@NIDs=1:length(xx@IDs)

  # TRes & TLim
  xx@TRes=ifelse(x@TRes<=y@TRes,x@TRes,y@TRes)

  T1=ifelse(x@TLim[1]<=y@TLim[1],x@TLim[1],y@TLim[1])
  T2=ifelse(x@TLim[2]>=y@TLim[2],x@TLim[2],y@TLim[2])
  xx@TLim=c(T1,T2)

  # Info
  Info13=lapply(x@Info, `[`, which(x@IDs%in%c(IDs1,IDs3)))
  Info2 =lapply(y@Info, `[`, which(y@IDs%in%IDs2))

  xx@Info=Map(c, Info13, Info2)

  # Pos
  xx@Pos=vector("list", length(xx@IDs))
  if(length(IDs1)>0){for (i in IDs1){ # Positions from ID1
      ii=which(xx@IDs==i)
      xx@Pos[[ii]]=x@Pos[[which(x@IDs==i)]] # Add position information
      xx@Pos[[ii]]$Time=(x@Pos[[which(x@IDs==i)]]$Time + x@TLim[1]) - xx@TLim[1] # Rebase time
    }}

  if(length(IDs2)>0){for (i in IDs2){ # Positions from ID2
      ii=which(xx@IDs==i)
      xx@Pos[[ii]]=y@Pos[[which(y@IDs==i)]] # Add position information
      xx@Pos[[ii]]$Time=(y@Pos[[which(y@IDs==i)]]$Time + y@TLim[1]) - xx@TLim[1] # Rebase time
    }}

  if(length(IDs3)>0){for (i in IDs3){ # Positions from ID1 & ID2
    ii=which(xx@IDs==i)

    # Add position information and rebase time
    xx@Pos[[ii]]$Time= c((x@Pos[[which(x@IDs==i)]]$Time + x@TLim[1]) - xx@TLim[1],
                         (y@Pos[[which(y@IDs==i)]]$Time + y@TLim[1]) - xx@TLim[1])

    dd=duplicated(xx@Pos[[ii]]$Time)
    if(sum(dd)>0) ddS=T
    xx@Pos[[ii]]$Time=xx@Pos[[ii]]$Time[!dd]
    xx@Pos[[ii]]$x   =c(x@Pos[[which(x@IDs==i)]]$x, y@Pos[[which(y@IDs==i)]]$x)[!dd]
    xx@Pos[[ii]]$y   =c(x@Pos[[which(x@IDs==i)]]$y, y@Pos[[which(y@IDs==i)]]$y)[!dd]

    # Sort positions
    oo=order(xx@Pos[[ii]]$Time)
    xx@Pos[[ii]]$Time=xx@Pos[[ii]]$Time[oo]
    xx@Pos[[ii]]$x   =xx@Pos[[ii]]$x[oo]
    xx@Pos[[ii]]$y   =xx@Pos[[ii]]$y[oo]
  }}
  names(xx@Pos)=xx@IDs
  if(ddS & verbose) message('Duplicated positions were found when merging. Obj1 (`x`) positions have been retained.')

  # Area Information
  n1=sapply(x@Area, `[[`, "ID") |> as.character()
  n2=sapply(y@Area, `[[`, "ID") |> as.character()
  n3=intersect(n1,n2)
  if(length(n3)>0  & verbose) message('Duplicated area information was found when merging. Obj1 (`x`) information has been retained.')

  a=setdiff(n2,n1)
  xx@Area=c(x@Area,y@Area[which(n2%in%a)])

  # Validate object and return
  if( !validate(xx))             stop("The merged AniSpace object is invalid.")
  return(xx)

})
