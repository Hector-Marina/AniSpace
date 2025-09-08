#' Loading bar
#'
#' @description
#' `sprog` displays a loading bar to show progress
#'
#' @param i loop number
#' @param n total number of loops
#'
#' @keywords loading bar progress
#'
#' @return print a loading bar to show progress
#'
#' @examples
#' pbar=sprog(50,100)
#' pbar
#'
#' @export

sprog=function(i,n){
  e=nchar('||100%')
  w=options()$width
  s=ceiling(i / n * (w - e))
  t=sprintf('|%s%s|% 3s%%', paste0(strrep('=', s-1),'>'),
            strrep(' ', w-s-e), round(i/n*100))
  cat(t)#;flush.console()
  if (i == n){cat('\n Done!')}else{cat('\r')}
}
