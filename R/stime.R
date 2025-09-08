#' Estimate time left
#'
#' @description
#' `stime` estimates the time required to complete a procedure
#'
#' @param i loop number
#' @param n total number of loops
#' @param st time at which the procedure started
#'
#' @keywords estimate time left
#'
#' @return displays the time required to complete a procedure
#'
#' @examples
#' st=Sys.time()
#' Sys.sleep(5)
#' tt=stime(50,100,st)
#' tt
#'
#' @export

stime=function(i,n,st){
  # Requires `st=Sys.time()` before the loop
  tt=difftime(Sys.time(), st, units='mins')
  cat(paste('Estimated time', round((tt/i)*(n-i),2), 'mins.  '))
  cat(if (i == n) '\n Done!' else '\r')
}
