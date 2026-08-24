#' Validate AniSpace object's information
#'
#' @description
#' `validate()` Verifies and Consistency Checks for AniSpace Objects
#'
#' @param AniObj Performs a series of integrity and quality-control checks on an
#' `AniSpace` object or dataset, ensuring that positional, animal,
#' and area data meet expected structural and logical criteria.
#'
#' @keywords internal
#'
#' @examples
#' validate(AniObj)
#'
#' @export
#'
validate=function(AniObj){
  VAL=TRUE

  # Validate NIDs IDs Info and Pos
  l=c(length(AniObj@NIDs),length(AniObj@IDs),length(AniObj@Pos))
  if(any(stats::dist(l)>0)){
    VAL=FALSE
    message("AniObj validation: Individual identifiers are inconsistent with the position data.")
  }

  # Validate NIDs and Info
  if(length(AniObj@Info)>0){
    ll=length(AniObj@NIDs)
    l=sapply(seq_along(AniObj@Info), function(ii) {length(AniObj@Info[[ii]])})
    if(!all(ll==l)){
      VAL=FALSE
      message("AniObj validation: Individual identifiers are inconsistent with individuals' information.")
    }
  }

  # Validate TLim and TRes
  if(length(AniObj@TLim)!=2)        {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}
  if(!is.numeric(AniObj@TLim))      {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}
  if(AniObj@TLim[2]<AniObj@TLim[1]) {message("AniObj validation: Time range information is incorrect.");VAL=FALSE}

  if(!is.numeric(AniObj@TRes) )     {message("AniObj validation: Time resolution information is incorrect.");VAL=FALSE}
  if(           AniObj@TRes<=0)     {message("AniObj validation: Time resolution information is incorrect.");VAL=FALSE}

  # Validate Pos
  l=sapply(seq_along(AniObj@Pos), function(ii) {
    length(AniObj@Pos[[ii]]$Time)== length(AniObj@Pos[[ii]]$x) &&
      length(AniObj@Pos[[ii]]$Time)== length(AniObj@Pos[[ii]]$y)})
  if(!all(l) )  {message("AniObj validation: Position information is incorrect (different lengths detected).");VAL=FALSE}
  l=sapply(seq_along(AniObj@Pos), function(ii) {
    is.numeric(AniObj@Pos[[ii]]$Time) && is.numeric(AniObj@Pos[[ii]]$x) && is.numeric(AniObj@Pos[[ii]]$y) })
  if(!all(l) )  {message("AniObj validation: Position information is incorrect (non-numeric information detected).");VAL=FALSE}

  # Validate Area
  l=sapply(seq_along(AniObj@Area), function(ii) {length(AniObj@Area[[ii]])})
  if(any(stats::dist(l)>0)){
    VAL=FALSE
    message("AniObj validation: Area information is inconsistent.")
  }
  l=sapply(seq_along(AniObj@Area), function(ii) {is.character(AniObj@Area[[ii]]$ID)})
  if(!all(l) )  {message("AniObj validation: Area ID information is incorrect (non-character information detected)."); VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {is.matrix(AniObj@Area[[ii]]$coords)})
  if(!all(l) )  {message("AniObj validation: Area coords information is incorrect (non-matrix information detected).");VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {
    c=AniObj@Area[[ii]]$coords
    ifelse(!is.matrix(c) || ncol(c) != 2L  || nrow(c) < 4L || (nrow(c) >= 1L && !all(c[1, ] == c[nrow(c), ])),FALSE,TRUE)
  })
  if(!all(l) )  {message("AniObj validation: Area coords are incorrect or incorrect (non-matrix information detected).");VAL=FALSE}

  l=sapply(seq_along(AniObj@Area), function(ii) {is.character(AniObj@Area[[ii]]$color)})
  if(!all(l) )  {message("AniObj validation: Area color information is incorrect (non-character information detected).");VAL=FALSE}


  # Validate SI (Spatial interaction) object
  if (!is.list(AniObj@SI)) {message("AniObj validation: Spatial-interaction information is incorrect."); VAL=FALSE}
  if (is.list(AniObj@SI) && length(AniObj@SI) > 0L) {

    if ( is.null(AniObj@SI$method)      || !is.character(AniObj@SI$method) ||
         length(AniObj@SI$method) != 1L || !AniObj@SI$method %in% c("time", "nint", "all")) {
                                                message("AniObj validation: Spatial-interaction method is incorrect."); VAL=FALSE}

    if (!is.null(AniObj@SI$dist.thr)){
      if (!is.numeric(AniObj@SI$dist.thr) || length(AniObj@SI$dist.thr) != 1L ||
          !is.finite(AniObj@SI$dist.thr)  || AniObj@SI$dist.thr <= 0) {
                                                message("AniObj validation: Spatial-interaction dist.thr is incorrect."); VAL=FALSE}}

    if (!is.null(AniObj@SI$time.thr)){
      if (!is.numeric(AniObj@SI$time.thr) || length(AniObj@SI$time.thr) != 1L ||
          !is.finite(AniObj@SI$time.thr)  || AniObj@SI$time.thr <= 0) {
                                                message("AniObj validation: Spatial-interaction time.thr is incorrect."); VAL=FALSE}}

    if(!is.matrix(AniObj@SI$M) ||
       ncol(AniObj@SI$M) != nrow(AniObj@SI$M)) {message("AniObj validation: Spatial-interaction matrix is incorrect."); VAL=FALSE}

  }

  # Validate UD (Utilisation distribution) object
  if (!is.list(AniObj@UD)) {message("AniObj validation: Utilisation-distribution information is incorrect."); VAL=FALSE}
  if (is.list(AniObj@UD) && length(AniObj@UD)>0L) {

    if (is.null(AniObj@UD$method)      || !is.character(AniObj@UD$method) ||
        length(AniObj@UD$method)!=1L  || !AniObj@UD$method %in% c("grid","poly","HR")) {
      message("AniObj validation: Utilisation-distribution method is incorrect."); VAL=FALSE}

    if (is.null(AniObj@UD$parameters) || !is.list(AniObj@UD$parameters)) {
      message("AniObj validation: Utilisation-distribution parameters are incorrect."); VAL=FALSE}

    if (is.null(AniObj@UD$UD) || !is.list(AniObj@UD$UD) ||
        length(AniObj@UD$UD)!=length(AniObj@IDs)) {
      message("AniObj validation: Utilisation-distribution results are incorrect."); VAL=FALSE}

    if (is.list(AniObj@UD$parameters) && !is.null(AniObj@UD$method) && AniObj@UD$method %in% c("grid","poly")) {

      xb=AniObj@UD$parameters$xbreaks
      yb=AniObj@UD$parameters$ybreaks

      if (!is.numeric(xb) || length(xb)<2L || any(!is.finite(xb)) ||
          any(diff(xb)<=0)) {
        message("AniObj validation: Utilisation-distribution xbreaks are incorrect."); VAL=FALSE}

      if (!is.numeric(yb) || length(yb)<2L || any(!is.finite(yb)) ||
          any(diff(yb)<=0)) {
        message("AniObj validation: Utilisation-distribution ybreaks are incorrect."); VAL=FALSE}
    }

    if (is.list(AniObj@UD$UD) && length(AniObj@UD$UD)==length(AniObj@IDs) &&
        !is.null(AniObj@UD$method)) {

      if (AniObj@UD$method=="grid"){
        ok=vapply(AniObj@UD$UD,function(x){
          is.null(x) ||
            (is.data.frame(x) &&
               all(c("cell","z") %in% names(x)) &&
               nrow(x)>0L &&
               is.numeric(x$cell) && all(is.finite(x$cell)) &&
               all(x$cell>0) && !anyDuplicated(x$cell) &&
               is.numeric(x$z) && all(is.finite(x$z)) &&
               all(x$z>=0) && sum(x$z)>0)
        },logical(1))

        if(!all(ok)){
          message("AniObj validation: Grid utilisation-distribution results are incorrect."); VAL=FALSE}
      }

      if (AniObj@UD$method=="poly"){
        ok=vapply(AniObj@UD$UD,function(x){
          if(is.null(x)) return(TRUE)
          if(!is.data.frame(x) || !all(c("x","y") %in% names(x)) ||
             nrow(x)<4L || !is.numeric(x$x) || !is.numeric(x$y) ||
             any(!is.finite(x$x)) || any(!is.finite(x$y))) return(FALSE)

          isTRUE(all.equal(x$x[1L],x$x[nrow(x)])) &&
            isTRUE(all.equal(x$y[1L],x$y[nrow(x)]))
        },logical(1))

        if(!all(ok)){
          message("AniObj validation: Polygon utilisation-distribution results are incorrect."); VAL=FALSE}
      }

      if (AniObj@UD$method=="HR"){
        ok=vapply(AniObj@UD$UD,function(x)
          is.null(x) || inherits(x,"estUD"),logical(1))

        if(!all(ok)){
          message("AniObj validation: Kernel home-range results are incorrect."); VAL=FALSE}
      }
    }
  }


  # Validate UDsim (Utilisation-distribution similarity) object
  if (!is.list(AniObj@UDsim)) {message("AniObj validation: Utilisation-distribution similarity information is incorrect."); VAL=FALSE}
  if (is.list(AniObj@UDsim) && length(AniObj@UDsim)>0L) {

    if (is.null(AniObj@UDsim$method)     || !is.character(AniObj@UDsim$method) ||
        length(AniObj@UDsim$method)!=1L  || !AniObj@UDsim$method %in% c("grid","poly","HR")) {
      message("AniObj validation: Utilisation-distribution similarity method is incorrect."); VAL=FALSE}

    if (is.null(AniObj@UDsim$sim.method)     || !is.character(AniObj@UDsim$sim.method) ||
        length(AniObj@UDsim$sim.method)!=1L  ||
        !AniObj@UDsim$sim.method %in% c("binary","continuous")) {
      message("AniObj validation: Utilisation-distribution similarity type is incorrect."); VAL=FALSE}

    if (is.null(AniObj@UDsim$HR.level)     || !is.numeric(AniObj@UDsim$HR.level) ||
        length(AniObj@UDsim$HR.level)!=1L  || !is.finite(AniObj@UDsim$HR.level) ||
        AniObj@UDsim$HR.level<=0           || AniObj@UDsim$HR.level>=100) {
      message("AniObj validation: Utilisation-distribution similarity HR.level is incorrect."); VAL=FALSE}

    if (!is.matrix(AniObj@UDsim$M) || !is.numeric(AniObj@UDsim$M) ||
        nrow(AniObj@UDsim$M)!=length(AniObj@IDs) ||
        ncol(AniObj@UDsim$M)!=length(AniObj@IDs)) {
      message("AniObj validation: Utilisation-distribution similarity matrix is incorrect."); VAL=FALSE}

    if (is.matrix(AniObj@UDsim$M) &&
        all(dim(AniObj@UDsim$M)==length(AniObj@IDs))) {

      M=AniObj@UDsim$M

      if (is.null(rownames(M)) || is.null(colnames(M)) ||
          !identical(rownames(M),AniObj@IDs) ||
          !identical(colnames(M),AniObj@IDs)) {
        message("AniObj validation: Utilisation-distribution similarity matrix names are incorrect."); VAL=FALSE}

      if (any(!is.na(M) & (!is.finite(M) | M<0 | M>1))) {
        message("AniObj validation: Utilisation-distribution similarity values are incorrect."); VAL=FALSE}

      if (is.list(AniObj@UD) && length(AniObj@UD)>0L &&
          is.list(AniObj@UD$UD) &&
          length(AniObj@UD$UD)==length(AniObj@IDs)) {

        valid=vapply(AniObj@UD$UD,function(x) !is.null(x),logical(1))
        expected=ifelse(valid,1,NA_real_)

        if(!isTRUE(all.equal(diag(M),expected,tolerance=1e-8))) {
          message("AniObj validation: Utilisation-distribution similarity matrix diagonal is incorrect."); VAL=FALSE}
      }

      if (!is.null(AniObj@UDsim$method) &&
          !is.null(AniObj@UDsim$sim.method) &&
          AniObj@UDsim$method=="HR" &&
          AniObj@UDsim$sim.method=="continuous" &&
          !isTRUE(all.equal(M,t(M),tolerance=1e-8))) {
        message("AniObj validation: Continuous home-range similarity matrix must be symmetric."); VAL=FALSE}
    }

    if (is.list(AniObj@UD) && length(AniObj@UD)>0L &&
        !is.null(AniObj@UD$method) &&
        !is.null(AniObj@UDsim$method) &&
        !identical(AniObj@UD$method,AniObj@UDsim$method)) {
      message("AniObj validation: UD and UDsim methods do not correspond."); VAL=FALSE}
  }


  return(VAL)
}
