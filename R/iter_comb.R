#' itertools.chain.
#' 
#' @param iter
#' @param func
#' @return list or vector
#' @export
#' @examples
#' chain(c(1, 2, 3), c(1, 2, 3))
chain <- function(...) {
  return(merge(...))
}