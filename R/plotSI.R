#' Plot pairwise spatial-interaction matrices and networks
#'
#' @description
#' `plotSI` plots pairwise spatial-interaction information stored in
#' the `SI` slot of an `AniSpace` object.
#'
#' @param AniObj An object of class `AniSpace` containing a pairwise
#' spatial-interaction adjacency matrix stored in the `SI` slot.
#' @param NIDs A numeric vector indicating the numerical identifiers of the
#' individuals to include in the plot. When `NULL`, all individuals are
#' included (*Default: NULL*).
#' @param IDs A character vector indicating the identifiers of the individuals
#' to include in the plot. When `NULL`, selection is based on `NIDs`
#' (*Default: NULL*).
#' @param type A character value specifying the type of plot. Available options
#' are `"matrix"` and `"network"`. `"matrix"` plots the spatial-interaction
#' adjacency matrix using `image`, whereas `"network"` plots the corresponding
#' interaction network using `igraph` (*Default: "matrix"*).
#' @param method A variable indicating the type of proximity interactions to export (`time`, `nint` or `mean`) (*Default: "time"*).
#' `time` will export the duration per pair of animals in closer proximity than the specified `dist.thr` threshold.
#' `nint` will export the number of instances where each pair of animals got within the specified `dist.thr` threshold of each other.
#' `mean` estimates the average of time interacted per instance.
#' @param int.thr A positive numeric value specifying the numeric threshold that
#' be used to dichotomise the interaction information (*Default: NULL*).
#' @param group A vector containing the group assigned to each selected
#' individual. Groups are represented by different vertex colours in the
#' network plot. When `NULL`, all individuals belong to the same group
#' (*Default: NULL*).
#' @param verbose A logical variable indicating whether the function will print
#' relevant information (`TRUE`) or not (`FALSE`) (*Default: TRUE*).
#'
#' @return Plot the adjacency matrix when `type = "matrix"` or the
#' corresponding `igraph` object when `type = "network"`.
#'
#' @examples
#' # Estimate spatial interactions
#' df.SI=spatialInt(AniObj)
#' df.SI@SI$M[1:5,1:5]
#'
#' # Plot the spatial-interaction adjacency matrix
#' plotSI(df.SI,type="matrix")
#'
#' # Plot the spatial-interaction network
#' plotSI(df.SI,NIDs=c(3:7),type="network")
#'
#' @export
#'

plotSI=function(AniObj, NIDs=NULL, IDs=NULL,
                         type="matrix", method="time",
                         int.thr=NULL, group=NULL,
                         verbose=TRUE) {

  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.", call. = FALSE)
  if( !validate(AniObj))             stop("Invalid `AniObj` object.", call. = FALSE)

  if (length(AniObj@SI) == 0L) stop("No spatial interaction results were found in `AniObj`.", call. = FALSE)

  if(is.null(NIDs) & is.null(IDs)){
    i=AniObj@NIDs
  }else if(!is.null(NIDs)){
    i=NIDs
  }else if(!is.null(IDs)){
    i=which(AniObj@IDs%in%IDs)
  }
  if (!is.null(i)){ if(!any(i%in%AniObj@NIDs)) stop("Individuals not found in `AniObj`")}
  if(length(i)<2L) stop("The number of individuals `NIDs` needs to be larger than one.")

  if (!type %in% c("matrix", "network"))         stop("Invalid `type`: must be one of 'matrix', or 'network'.")
  if (!method %in% c("time", "nint", "mean"))  stop("Invalid `method`: must be one of 'time', 'nint', or 'mean'.")
  if(method=="mean" && AniObj@SI$method!="all" ) stop("`time` or `nint` information was not estimated in spatialint.AniSpace().")
  if(method=="time" && AniObj@SI$method=="nint") stop("`time` information was not estimated in spatialint.AniSpace().")
  if(method=="nint" && AniObj@SI$method=="time") stop("`nint` information was not estimated in spatialint.AniSpace().")

  if (!is.null(int.thr)){
    if (!is.numeric(int.thr) || length(int.thr) != 1L ||
        !is.finite(int.thr)  || int.thr <= 0)  stop("`int.thr` must be a single positive numeric value.")
  }

  if (!is.null(group)){
    if (length(group)!=length(i)) stop("`group` must have one value per selected individual.")
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

  # Extract interaction information
  M=as.matrix(AniObj@SI$M)
  M=M[i, i, drop = FALSE]
  rownames(M)=colnames(M)=AniObj@IDs[i]

  if(method=="time") M[lower.tri(M)] = t(M)[lower.tri(M)]
  if(method=="nint") M[upper.tri(M)] = t(M)[upper.tri(M)]
  if(method=="mean") {
    M = M/t(M)
    M[lower.tri(M)] = t(M)[lower.tri(M)]
    M[is.nan(M)]=0}

  if (!is.null(int.thr)){
    M[M<int.thr]=0
    M[M>0]=1
  }

  if(all(M==0)) stop("After the filters applied the pairwise spatial-interaction matrix contains not proximity contacts.")

  if (type=="matrix"){
    if (verbose) message("- Plotting the spatial-interaction matrix for ", nrow(M), " individual(s).")

    n=nrow(M)
    M.plot=M[n:1L, , drop = FALSE]

    graphics::par(mar=c(2,7,7,7),pty="s")
    graphics::image(x = seq_len(n), y = seq_len(n),
      z = t(M.plot), axes = FALSE,
      xlab = "", ylab = "", asp = 1)

    graphics::axis(side = 3,at = seq_len(n),labels = colnames(M),
      las = 2, tick = TRUE)

    graphics::axis(side = 2, at = seq_len(n), labels = rev(rownames(M)),
      las = 2, tick = TRUE)

    graphics::box()
  }

  #---
  if (type == "network") {
    if (verbose) message("- Plotting the spatial-interaction network for ", nrow(M), " individual(s).")

    # Create weighted undirected network
    g=igraph::graph_from_adjacency_matrix(M, mode = "undirected",
                                          weighted = TRUE, diag = FALSE)


    # Scale edge widths according to interaction values
    edge.weight=igraph::E(g)$weight

    if (length(unique(edge.weight)) == 1L) {
      edge.width=2
    } else {
      edge.width=1+10*(
        edge.weight-min(edge.weight) )/( max(edge.weight)-min(edge.weight))
    }

    # Assign colours to groups
    if (is.null(group)) {
      group=factor(rep("All",length(i)))
    }else{
      if (!is.factor(group)) group=as.factor(group)
      group=droplevels(group)
    }

    colPal=stats::setNames(grDevices::rainbow(length(unique(group))), unique(group))
    vertex.col=unname(colPal[group])

    # Plot using the igraph plot method
    graphics::plot(
      g,
      vertex.color = vertex.col,
      vertex.label = NA,#AniObj@IDs[i],
      #vertex.label.cex = 0.8,
      vertex.size = 10,
      edge.width = edge.width)
    graphics::legend(
      "topright",
       legend=levels(group),
       pch=21,
       pt.bg=colPal[levels(group)],
       pt.cex=1.5,
       title="Groups",
       bty="n")
  }

}
