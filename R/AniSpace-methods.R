#' Show method for AniSpace
#'
#' @description
#' Displays a summary of the information contained in an `AniSpace` object.
#'
#' @param object An `AniSpace` object.
#'
#' @importMethodsFrom methods show
#'
#' @export
#'
methods::setMethod("show", signature = "AniSpace", function(object) {
  cat("-------AniSpace: Spatio-temporal object-------\n")

  # Individuals
  if (length(object@NIDs) > 5){
    cat("# Individuals:", length(object@NIDs),"(", paste(utils::head(object@IDs,5), collapse = ", "), ",...)", "\n")
  } else {
    cat("# Individuals:", length(object@NIDs),"(", paste(object@IDs, collapse = ", "),")", "\n")
  }

  # Information
  if (length(object@Info)>0){
    if (length(object@Info) > 5){
      cat("# Traits:", length(object@Info),"(", paste(utils::head(names(object@Info),5), collapse = ", "), ",...)", "\n")
    } else {
      cat("# Traits:", length(object@Info),"(", paste(names(object@Info), collapse = ", "),")", "\n")
    }
  }

  # Areas
  if (length(object@Area)>0){
    n=sapply(object@Area, `[[`, "ID") |> as.character()
    if (length(object@Area) > 5){
      cat("# Areas:", length(object@Area),"(", paste(utils::head(n,5), collapse = ", "), ",...)", "\n")
    } else {
      cat("# Areas:", length(object@Area),"(", paste(n, collapse = ", "),")", "\n")
    }
  }

  # Positions
  x=sapply(object@Pos, function(p) length(p$x))
  cat("# Positions per ID: mean=",round(mean(x),2),"; sd=",round(stats::sd(x),2), "\n")
  cat("% missing positions ( Time resolution=",epoch2time(object@TRes),"):",
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

  # Spatial interaction matrix
  if (length(object@SI)>0){
    cat("\u00A7 Spatial interaction matrix: ", nrow(object@SI$M), "individuals available. \n")
    if(!is.null(object@SI$method))   cat("  Method:", object@SI$method, "\n")
    if(!is.null(object@SI$dist.thr)) cat("  Distance threshold:", object@SI$dist.thr, "\n")
    if(!is.null(object@SI$time.thr)) cat("  Time threshold:    ", object@SI$time.thr, "\n")
  }

  # Area utilisation distribution information
  if (length(object@UD)>0){
    cat("\u00A4 Utilisation distribution information for:", sum(!vapply(object@UD$UD, is.null, logical(1))),
        "individuals out of",length(object@UD$UD),"available. \n")
    cat("  Utilisation distribution method: ", object@UD$method, "\n")
  }

  # Area utilisation distribution similarity
  if (length(object@UDsim)>0){
    cat("\u00A7 Utilisation distribution similarity matrix: ", nrow(object@UDsim$M), "individuals available. \n")
    if(!is.null(object@UDsim$method))       cat("  Method:", object@UDsim$method, "\n")
    if(!is.null(object@UDsim$sim.method))   cat("  Similarity method:", object@UDsim$sim.method, "\n")
    if(!is.null(object@UDsim$method) && object@UDsim$method=="HR"){
      cat("  HomeRange threshold (HR.level):", object@UDsim$HR.level, "\n")
    }
  }
})




#' Names method for AniSpace
#'
#' @description
#' Returns the slot names of an `AniSpace` object.
#'
#' @param x An `AniSpace` object.
#'
#' @return A character vector containing the slot names of the object.
#'
#' @importFrom methods slotNames
#'
#' @export
#'
methods::setMethod("names", signature = "AniSpace", function(x) slotNames(x))



#' Merge two AniSpace objects
#'
#' @description
#' Merges two `AniSpace` objects, combining their individual information,
#' positions and area information. When the same individual is present in both
#' objects, position information is combined and duplicated time points are
#' removed, retaining the information from `x`.
#'
#' Spatial-interaction, utilisation-distribution and utilisation-distribution
#' similarity results are removed after merging because they must be
#' re-estimated from the merged position information.
#'
#' @param x An `AniSpace` object.
#' @param y An `AniSpace` object to be merged with `x`.
#' @param verbose A logical value indicating whether informative messages are
#' printed (*Default: TRUE*).
#' @param ... Additional arguments. Currently ignored.
#'
#' @return An `AniSpace` object containing the merged information.
#'
#' @examples
#' # Filter the AniSpace Object
#' df1=filterAniSpace(AniObj,NIDs=c(1:5))
#' df2=filterAniSpace(AniObj,NIDs=c(1:5))
#'
#' # Merge the AniSpace Object
#' df.merge=merge(df1,df2)
#' df.merge
#'
#' @export
#'
methods::setMethod("merge", signature = c(x = "AniSpace", y = "AniSpace"), function(x, y, verbose=TRUE,...){

  if (!inherits(x, "AniSpace")) stop("`x` must be class 'AniSpace'.")
  if( !validate(x))             stop("Invalid `x` object.")
  if (!inherits(y, "AniSpace")) stop("`y` must be class 'AniSpace'.")
  if( !validate(y))             stop("Invalid `y` object.")
  if(!is.logical(verbose)) stop("`verbose` must be logical")


  ddS=FALSE # Duplicate switches

  # Create the AniSpace object
  xx=methods::new("AniSpace",
         NIDs = as.numeric(),
         IDs  = as.character(),
         Info = list(),
         TLim = as.numeric(),
         TRes = as.numeric(),
         Pos  = list(),
         Area = list(),
         SI = list(),
         UD = list(),
         UDsim = list())

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

  # SI Information
  if(length(x@SI)>0L || length(y@SI)>0L){
    if(verbose)
      message("Spatial-interaction information was removed after merging and needs to be re-estimated.")
    xx@SI=list()
  }

  # UD Information
  if(length(x@UD)>0L || length(y@UD)>0L){
    if(verbose)
      message("Utilisation-distribution information was removed after merging and needs to be re-estimated.")
    xx@UD=list()
  }

  # UD similarity Information
  if(length(x@UDsim)>0L || length(y@UDsim)>0L){
    if(verbose)
      message("Utilisation-distribution similarity information was removed after merging and needs to be re-estimated.")
    xx@UDsim=list()
  }

  # Validate object and return
  if( !validate(xx))             stop("The merged AniSpace object is invalid.")
  return(xx)

})



#' Plot method for AniSpace
#'
#' @description
#' Plots the spatial position information contained in an `AniSpace` object.
#'
#' @param x An `AniSpace` object.
#' @param y Missing.
#' @param ... Additional arguments passed to `plotAniSpace()`.
#'
#' @importFrom graphics plot
#'
#' @export
#'
methods::setMethod("plot",
                   signature(x="AniSpace",y="missing"),
                   function(x,y,...){
                     plotAniSpace(AniObj=x,...)
                   })


#' as.data.frame method for AniSpace
#'
#' @description
#' Converts the position information stored in an `AniSpace` object into a data frame.
#'
#' @param x An `AniSpace` object.
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#' @param ... Additional arguments. Currently ignored.
#'
#' @return A data frame containing the individual IDs and position information.
#'
#' @export
#'
methods::setMethod("as.data.frame",signature(x="AniSpace"),function(x,verbose=TRUE,...){

  if(!validate(x)) stop("Invalid `AniSpace` object.")

  l=lapply(seq_along(x@Pos),function(ii){
    data.frame(
      NIDs=x@NIDs[ii],
      IDs=x@IDs[ii],
      x@Pos[[ii]]
    )
  })

  Pos_info=do.call(rbind,l)

  if (verbose) {
    message("Information on positions exported to the data frame:")
    message("- Total number of individuals: ", length(x@NIDs))
    message("- Total number of records: ", nrow(Pos_info))
  }

  return(Pos_info)
})


#' as.list method for AniSpace
#'
#' @description
#' Converts an `AniSpace` object into a list containing its slots.
#'
#' @param x An `AniSpace` object.
#' @param verbose A logical variable specifying whether to print informative messages (*Default: TRUE*).
#' @param ... Additional arguments. Currently ignored.
#'
#' @return A list containing the information stored in the `AniSpace` object.
#'
#' @export
#'
methods::setMethod("as.list",signature(x="AniSpace"),function(x,verbose=TRUE,...){

  # Control parameters
  if( !validate(x))             stop("Invalid `AniObj` object.")

  # Extract animal information to data frame
  if(!is.null(x@Info) && length(x@Info) > 0){
    IDs_info=data.frame(NIDs=x@NIDs,IDs=x@IDs,x@Info)
    if (verbose) {
      message("Information on animals exported to the list:")
      message("- Total number of traits: ", ncol(IDs_info)-2)
    }
  }else{IDs_info=data.frame()}

  # Extract position information to data frame
  l=lapply(seq_along(x@Pos), function(ii) {
    data.frame(NIDs=x@NIDs[ii],IDs=x@IDs[ii],x@Pos[[ii]])
  })
  Pos_info=do.call(rbind, l)
  if (verbose) {
    message("Information on positions exported to the list:")
    message("- Total number of individuals: ", length(x@NIDs))
    message("- Total number of records: ",     nrow(Pos_info))
  }

  # Extract area information to data frame
  if(!is.null(x@Area) && length(x@Area) > 0){
    l=lapply(seq_along(x@Area), function(ii) {
      data.frame(ID=x@Area[[ii]]$ID,
                 minX=min(x@Area[[ii]]$coords[,"x"]), maxX=max(x@Area[[ii]]$coords[,"x"]),
                 minY=min(x@Area[[ii]]$coords[,"y"]), maxY=max(x@Area[[ii]]$coords[,"y"]),
                 color=x@Area[[ii]]$color)})
    Area_info=do.call(rbind, l)
    if (verbose) {
      message("Information on areas exported to the list:")
      message("- Total number of areas: ", length(x@NIDs))
    }
  }else{Area_info=data.frame()}

  return(list(IDs_info=IDs_info,Pos_info=Pos_info,Area_info=Area_info))
})
