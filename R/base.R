#' split single element character vector to each character
#' 
#' @param ... iterables
#' @return list or vector
#' @examples
#' .preprocess('AAA')
#' .preprocess(c('A', 'A', 'A'))
.preprocess <- function(iterable) {
  if (length(iterable) == 1 && is.character(iterable)) {
    iterable <- unlist(strsplit(iterable, ''))
  }
  return(iterable)
}