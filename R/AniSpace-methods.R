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



  coords=do.call(rbind, lapply(seq_along(object@Area), function(ii) object@Area[[ii]]$coords[, c("x","y"), drop = FALSE]))
  xlim=range(coords[, "x"], na.rm = TRUE)
  ylim=range(coords[, "y"], na.rm = TRUE)

  cat("  Position bounds: x=c(", round(xlim[1],2),",",round(xlim[2],2),"); y=c(", round(ylim[1],2),",",round(ylim[2],2), ")\n")

  # Time span
  cat("  Time span:", object@TLim[1],"(",as.character(as.POSIXct(object@TLim[1], origin = "1970-01-01", tz="UTC")),") -",
                      object@TLim[2],"(",as.character(as.POSIXct(object@TLim[2], origin = "1970-01-01", tz="UTC")),") \n")
  cat("  Durantion:", epoch2time(object@TLim[2]-object@TLim[1]), "\n")

})


# 2) names method for AniSpace: make names(AniSpace) return the slotNames
#'
#' @export
setMethod("names", signature = "AniSpace", function(x) slotNames(x))





#' #' 6) merge function for AniSpace
#' #' #'
#' #' @param AniObj1 An AniSpace object
#' #' @param AniObj2 An AniSpace object
#' #'
#' #' @examples
#' #' df_merged=merge.AniSpace(df, d2)
#' #'
#' #' @export
#' merge.AniSpace=function(AniObj1, AniObj2) {
#'
#'   if (!inherits(AniObj1, "AniSpace")) stop("`AniObj1` must be class 'AniSpace'.")
#'   if (!inherits(AniObj2, "AniSpace")) stop("`AniObj2` must be class 'AniSpace'.")
#'
#'   stop("merge fuction not implemented yet")
#'
#'   Time Resolution must be the same to do the merge or the lowest?
#'
#'
#'   Time need to be reindexed according to the new Tlims
#'
#'   # Add Animal Position Information to the AniSpace object1
#'
#'   # Add Area Information to the AniSpace object1
#'   if(length(AniObj@Area)==0){
#'     AniObj@Area=AreaObj
#'     n=sapply(AreaObj, `[[`, "ID") |> as.character()
#'     if (verbose) message("- Areas: ", paste(n, collapse = ", "),"; have been added to the AniSpace object.")
#'   } else {
#'     n1=sapply(AniObj@Area, `[[`, "ID") |> as.character()
#'     n2=sapply(AreaObj, `[[`, "ID") |> as.character()
#'     a=setdiff(n2,n1)
#'
#'     if(length(a)>0){
#'       AniObj@Area=c(AniObj@Area,AreaObj[which(n2%in%a)])
#'       if (verbose) message("- Areas: ", paste(a, collapse = ", "),"; have been added to the AniSpace object.")
#'     }else{
#'       if (verbose) message("- No Area information has been added to the AniSpace object.")
#'     }
#'   }
#' }


#' #' 7) Convert AniSpace object into dataframes
#' #' #'
#' #' @param AniObj An AniSpace object
#' #'
#' #' @examples
#' #' df.list=Ani2tbl(df)
#' #'
#' #' @export
#'
#' Ani2tbl=function(AniObj){
#'
#'   IDs_info
#'   names(AniObj)
#'   [1] "NIDs" "IDs"  "Info"
#'
#'   Pos_info
#'   names(AniObj)
#'   "TLim" "TRes" "Pos"
#'
#'   stop("Include also Ani2tbl as a tool to extract the data as a dataframe.")
#'   int.obj=as.data.frame(AniObj@Pos[[ii]])
#'
#'   Area_info
#'   names(AniObj)
#'   "Area"
#'
#'   return(list(IDs_info,Pos_info,Area_info))
#' }
#'
#' }
#'
#' stop("Include also Ani2tbl as a tool to extract the data as a dataframe.")
#' int.obj=as.data.frame(AniObj@Pos[[ii]])
