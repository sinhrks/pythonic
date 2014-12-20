#' itertools.accumulate
#' 
#' @param p
#' @param func
#' @return list or vector
#' @export
#' @examples
#' iter.accumulate(c(1, 2, 3, 4, 5))
#' iter.accumulate(c('A', 'B', 'C'))
#' iter.accumulate(c(1, 2, 3, 4, 5), func = function(x, y) { x * y })
iter.accumulate <- function(p, func = NULL) {
  if (is.null(func)) {
    if (is.character(p)) {
      func <- function(x, y) { paste0(x, y) }
    } else {
      return(cumsum(p))
    }
  }
  result <- Reduce(func, p, accumulate=TRUE)
  if (is.list(p) && ! is.list(result)) {
    return(list(result))
  }
  return(result)
}

#' itertools.chain
#' 
#' @param ... iterables
#' @return list or vector
#' @export
#' @examples
#' iter.chain(c(1, 2, 3), c(1, 2, 3))
iter.chain <- function(...) {
  # ToDo: !!
  return(merge(...))
}

#' itertools.compress
#' 
#' @param data iterables
#' @param selectors iterables
#' @return list or vector
#' @export
#' @examples
#' iter.compress(c(1, 2, 3), c(TRUE, FALSE, TRUE))
iter.compress <- function(data, selectors) {
  return(data[selectors])
}

#' itertools.dropwhile
#' 
#' @param pred iterables
#' @param seq iterables
#' @return list or vector
#' @export
#' @examples
#' iter.dropwhile(function(x) { x > 2 }, c(1, 2, 3, 2, 1))
iter.dropwhile <- function(pred, seq) {
  inv <- function(x) { !pred(x) }
  pos <- Position(inv, seq, right = FALSE, nomatch = NA_integer)
  selectors <- c(rep(F, pos - 1), rep(T, length(seq) - pos + 1))
  return(iter.compress(seq, selectors))
}

#' itertools.takewhile
#' 
#' @param pred iterables
#' @param seq iterables
#' @return list or vector
#' @export
#' @examples
#' iter.takewhile(function(x) { x > 2 }, c(1, 2, 3, 2, 1))
iter.takewhile <- function(pred, seq) {
  inv <- function(x) { !pred(x) }
  pos <- Position(inv, seq, right = FALSE, nomatch = NA_integer)
  selectors <- c(rep(T, pos - 1), rep(F, length(seq) - pos + 1))
  return(iter.compress(seq, selectors))
}

#' itertools.filterfalse
#' 
#' @param pred iterables
#' @param seq iterables
#' @return list or vector
#' @export
#' @examples
#' iter.filterfalse(function(x) { x > 2 }, c(1, 2, 3, 2, 1))
iter.filterfalse <- function(pred, seq) {
  selectors <- !as.logical(pred(seq))
  return(iter.compress(seq, selectors))
}

#' itertools.groupby
#' 
#' @param iterable iterables
#' @param key
#' @return list or vector
#' @export
#' @examples
#' iter.chain(c(1, 2, 3), c(1, 2, 3))
iter.groupby <- function(...) {
  # ToDo: !!
  return(merge(...))
}

#' itertools.islice
#' 
#' @param iterable iterables
#' @param start
#' @param stop
#' @param step
#' @return list or vector
#' @export
#' @examples
#' iter.islice('ABCDEFG', 2)
#' iter.islice('ABCDEFG', 2, 4)
#' iter.islice('ABCDEFG', 2, NULL)
#' iter.islice('ABCDEFG', 0, NULL, 2)
iter.islice <- function(iterable, ...) {
  iterable <- .preprocess(iterable)
  n = nargs() - 1
  args <- list(...)
  if (n == 0) {
    stop('islice expected at least 2 arguments, got 1')
  } else if (n == 1) {
    start <- 1
    end <- args[[1]]
    step <- 1
  } else if (n == 2) {
    start <- args[[1]]
    end <- args[[2]]
    step <- 1    
  } else if (n == 3) {
    start <- args[[1]] 
    end <- args[[2]]
    step <- args[[3]]       
  } else {
    stop(paste0('islice expected at most 4 arguments, got ', n))
  }
  if (is.null(start)) {
    start <- 1
  }
  if (is.null(end)) {
    end <- length(iterable)
  }
  
  if (is.null(step)) {
    step <- 1
  }
  return(iterable[seq(start, end, step)])
}

#' itertools.starmap
#' 
#' @param func 
#' @param seq iterables
#' @return list or vector
#' @export
#' @examples
#' iter.starmap(function(x, y, z) { x + y + z }, list(c(1, 2, 3), c(2, 3, 4)))
iter.starmap <- function(func, seq) {
  f <- function(x) {
    if (length(x) > 1) {
      return(do.call(func, as.list(x)))
    } else {
      return(func(x))
    }
  }
  result <- lapply(seq, f)
  return(result)
}

