#' Fit dyadic interaction models to AniSpace data
#'
#' @description
#' `fitERGM` constructs pairwise data from the individual attributes
#' stored in `AniObj@Info` and the spatial-interaction information stored in
#' `AniObj@SI`. Depending on `int.thr`, the selected interaction measure is
#' analysed either as a continuous response or converted into a binary network.
#'
#' @param AniObj An `AniSpace` object containing individual information and
#' spatial-interaction results.
#' @param method A character value specifying the interaction measure used as
#' the response. Available options are `"time"`, and `"nint"`, and `"mean"`.
#' `"time"` uses the duration for which each pair of individuals was separated
#' by less than the distance threshold used to estimate spatial interactions.
#' `"nint"` uses the number of proximity-interaction events between each pair.
#' `"mean"` fits the corresponding average of duration of interaction per instance.
#' (*Default: "time"*).
#' @param int.thr Either `NULL` or a positive numeric value specifying the
#' threshold used to dichotomise the selected interaction measure. When `NULL`,
#' interaction values are transformed as `log(1+x)` and then standardised to
#' have mean 0 and standard deviation 1 before fitting a linear model. When
#' supplied, values equal to or greater than `int.thr` are coded as an
#' interaction and a binomial logistic model is fitted (*Default: 600*).
#' @param fix.eff Either `NULL` or a character vector containing the names of
#' individual-level variables stored in `AniObj@Info` to include as fixed
#' effects. For each selected variable, pairwise predictors are constructed for
#' every dyad and included in the fitted model. When `NULL`, pairwise predictors
#' are generated from all variables available in `AniObj@Info`
#' (*Default: NULL*).
#' @param verbose A logical value indicating whether informative messages are
#' printed (*Default: TRUE*).
#'
#' @return A list containing the pairwise model data in `data` and the fitted
#' model object or objects in `model`.
#'
#' @examples
#' # Filter individuals
#' df.ID.filt=filterAniSpace(AniObj,NIDs=c(1:7))
#'
#' # Estimate spatial interactions required for the dyadic models
#' df.SI=spatialInt(df.ID.filt,method="all",verbose=FALSE)
#'
#' # Estimate utilisation-distribution similarities
#' df.UD=areaUD(df.SI,method="grid",verbose=FALSE)
#' df.sim=UDsim(df.UD,verbose=FALSE)
#'
#' # Fit a linear model to the normalised interaction values
#' ergm.linear=fitERGM(df.sim,method="time",int.thr=NULL,
#'                     fix.eff=c("Parity","DIM"),verbose=FALSE)
#' head(ergm.linear$data)
#' summary(ergm.linear$model)
#'
#' # Fit a logistic model to dichotomised interaction values
#' df.sim.filt=filterAniSpace(df.sim,NIDs=c(3:7))
#' ergm.binary=fitERGM(df.sim.filt,method="nint",int.thr=10,
#'                   fix.eff=c("DIM"),verbose=FALSE)
#'
#' head(ergm.binary$data)
#' summary(ergm.binary$model)
#'
#' @export


fitERGM=function(AniObj, method="time", int.thr=600, fix.eff=NULL, verbose=TRUE){
  # Control parameters
  if (!inherits(AniObj, "AniSpace")) stop("`AniObj` must be class 'AniSpace'.")
  if( !validate(AniObj))             stop("Invalid `AniObj` object.")

  if (!method %in% c("time", "nint", "mean"))  stop("Invalid `method`: must be one of 'time', 'nint', or 'mean'.")
  if(method=="mean" && AniObj@SI$method!="all" ) stop("`time` or `nint` information was not estimated in spatialint.AniSpace().")
  if(method=="time" && AniObj@SI$method=="nint") stop("`time` information was not estimated in spatialint.AniSpace().")
  if(method=="nint" && AniObj@SI$method=="time") stop("`nint` information was not estimated in spatialint.AniSpace().")

  if (!is.null(int.thr)){
    if (!is.numeric(int.thr) || length(int.thr) != 1L ||
        !is.finite(int.thr)  || int.thr <= 0)  stop("`int.thr` must be a single positive numeric value.")
  }

  if(is.null(fix.eff)){
    fix.eff=names(AniObj@Info)
  }else{
    if(!is.character(fix.eff))
      stop("'fix.eff' must be NULL or a character vector.")

    if(length(fix.eff)==0L)
      stop("'fix.eff' cannot be an empty character vector.")

    if(anyNA(fix.eff) || any(fix.eff==""))
      stop("'fix.eff' cannot contain missing or empty values.")

    invalid=setdiff(fix.eff,names(AniObj@Info))

    if(length(invalid)>0L)
      stop(
        "The following variables supplied in 'fix.eff' are not present in ",
        "'AniObj@Info': ",paste(invalid,collapse=", "),"."
      )

    fix.eff=unique(fix.eff)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be either TRUE or FALSE.")

  #-- Extract SI information
  M=as.matrix(AniObj@SI$M)
  rownames(M)=colnames(M)=AniObj@IDs

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

  if(all(M==0 | is.na(M))) stop("After the filters applied the pairwise spatial-interaction matrix contains no proximity contacts.")

  # Convert to edge_list
  ii=which(upper.tri(M),arr.ind=TRUE)

  M=data.frame(value=M[ii],
    edge=paste(colnames(M)[ii[,1]],colnames(M)[ii[,2]],sep="_"),
    row.names=NULL)

  # Convert response variable to normal if int.thr=NULL
  if (is.null(int.thr)){
    M$value=as.numeric(scale(log1p(M$value)))
  }

  #-- Extract Info information
  IDs_info=data.frame(IDs=AniObj@IDs, AniObj@Info)
  IDs_info=IDs_info[,c("IDs",fix.eff),drop=FALSE]

  supported=vapply(IDs_info,function(x){
    is.factor(x) ||
      identical(class(x),"character") ||
      identical(class(x),"integer") ||
      identical(class(x),"numeric") ||
      identical(class(x),"logical")
  },logical(1))

  if(any(!supported)){
    removed=paste0(
      names(IDs_info)[!supported],
      " [",
      vapply(IDs_info[!supported],function(x)
        paste(class(x),collapse="/"),character(1)),
      "]"
    )
    if(verbose)
      message(
        "The following variables in AniObj@Info were excluded because ",
        "only character, integer, numeric and factor variables are supported: ",
        paste(removed,collapse=", "),
        "."
      )
  }

  IDs_info=IDs_info[,supported,drop=FALSE]

  if(ncol(IDs_info)==1L)
    warning(
      "No supported variables were found in AniObj@Info. ",
      "Supported classes are character, integer, numeric and factor."
    )

  l=lapply(2:ncol(IDs_info),function(i){

    x=IDs_info[[i]]
    v_name=names(IDs_info)[i]
    pairs=t(utils::combn(IDs_info[[i]],2L))

    if(is.character(x) || is.factor(x) || is.logical(x)){

      f=factor(x)
      lev=levels(f)

      if(length(lev)<2L){
        if(verbose) message("Variable '",v_name,
            "' was excluded because it contains fewer than two levels."
          )
        return(NULL)
      }

      #node.match
      M=ifelse(pairs[,1]==pairs[,2],1,0)

      #node.factor
      pairs.NA=rowSums(is.na(pairs))==0
      Z1=stats::model.matrix(~0+pairs[pairs.NA,1])
      Z2=stats::model.matrix(~0+pairs[pairs.NA,2])
      Z=Z1+Z2

      Fix=matrix(NA_integer_,nrow=nrow(pairs),ncol=(ncol(Z)))
      Fix[,1]=M
      Fix[pairs.NA,2:ncol(Fix)]=Z[,-1]

      colnames(Fix)=c(paste0("node.match_",v_name),
                      paste0("node.factor_",lev[-1L],"_",v_name))
      return(Fix)
    }

    if(is.integer(x) || is.numeric(x)){
      ECS=rowSums(pairs)
      ECA=abs(pairs[,1L]-pairs[,2L])

      Fix=cbind(ECS,ECA)

      colnames(Fix)=c(paste0("edge.cov.sum_",v_name),
                      paste0("edge.cov.abs_",v_name))

      return(Fix)
    }
    NULL
  })
  edge=t(utils::combn(IDs_info[[1]],2L))
  edge=paste(edge[,1],edge[,2],sep="_")

  Fix=cbind(edge,do.call(cbind,l))


  #--
  UDsim=as.matrix(AniObj@UDsim$M)
  rownames(UDsim)=colnames(UDsim)=AniObj@IDs

  if(all(UDsim==0 | is.na(UDsim))) stop("The pairwise utilisation distribution contains no interaction.")

  # Convert to edge_list
  ii=which(upper.tri(UDsim), arr.ind=TRUE)

  UDsim=data.frame(UDsim=UDsim[ii],
                   edge=paste(colnames(UDsim)[ii[,1]],colnames(UDsim)[ii[,2]],sep="_"),
                   row.names=NULL)

  # Combine
  data=cbind(M,Fix[,-1],UDsim[,-2])

  #-- Run model.
  valid=stats::complete.cases(data)
  if(!any(valid))
    stop("No complete cases remain for the selected fixed effects.")

  # Create X with intercept
  X=data.frame(Fix[,-1,drop=FALSE], UDsim=UDsim[,1], check.names=FALSE)
  X[]=lapply(X,function(x) as.numeric(as.character(x)))
  X=as.matrix(X)
  storage.mode(X)="double"
  X=X[valid,]
  X=cbind("(Intercept)"=1,X)

  # Create response variable
  y=as.numeric(M[valid,1])


  # Run the models
  if(is.null(int.thr)){
    model=stats::glm.fit(x=X, y=y, family=stats::gaussian(link="identity"))
    class(model)=c("glm","lm")
  }else{
    if(length(unique(y))<2L)
      stop("No positive interaction values remain after removing missing data.")
    model=stats::glm.fit(x=X, y=y, family=stats::binomial(link="logit"))
    class(model)=c("glm","lm")
  }

  return(list(data=data,model=model))
}
