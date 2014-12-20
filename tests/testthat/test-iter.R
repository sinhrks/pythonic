context('test iterators')

test_that('accumulate: basics', {
  
  result <- iter.accumulate(c(1, 2, 3, 4, 5))
  expect_equal(result, c(1, 3, 6, 10, 15))

  result <- iter.accumulate(c('A', 'B', 'C'))
  expect_equal(result, c('A', 'AB', 'ABC'))
  
  result <- iter.accumulate(c(1, 2, 3, 4, 5), func = function(x, y) { x * y })
  expect_equal(result, c(1, 2, 6, 24, 120))

  result <- iter.accumulate(list(1, 2, 3, 4, 5))
  expect_equal(result, c(1, 3, 6, 10, 15))
 
  result <- iter.accumulate(list(c(1, 2), c(3, 4), c(5)), function(x, y) { c(x, y) })
  expect_equal(result, list(c(1, 2), c(1, 2, 3, 4), c(1, 2, 3, 4, 5)))
})

test_that('accumulate: python examples', {
  
  data <- c(3, 4, 6, 2, 1, 9, 0, 7, 5, 8)
  result <- iter.accumulate(data, function(x, y) { x * y })
  expect_equal(result, c(3, 12, 72, 144, 144, 1296, 0, 0, 0, 0))
  
  result <- iter.accumulate(data, max)
  expect_equal(result, c(3, 4, 6, 6, 6, 9, 9, 9, 9, 9))
  
  # Amortize a 5% loan of 1000 with 4 annual payments of 90
  cashflows = c(1000, -90, -90, -90, -90)
  result <- iter.accumulate(cashflows, function(bal, pmt) { bal * 1.05 + pmt })
  expect_equal(result, c(1000, 960.0, 918.0, 873.9000000000001, 827.5950000000001))
  
  # Chaotic recurrence relation http://en.wikipedia.org/wiki/Logistic_map
  logistic_map <-  function(x, y) { r * x * (1 - x) }
  r <- 3.8
  x0 <- 0.4
  inputs = rep(x0, 36)     # only the initial value is used
  result <- iter.accumulate(inputs, logistic_map)
  result <- formatC(result, format = 'f', digits = 2)
  expect_equal(result,  c('0.40', '0.91', '0.30', '0.81', '0.60', '0.92',
                          '0.29', '0.79', '0.63', '0.88', '0.39', '0.90',
                          '0.33', '0.84', '0.52', '0.95', '0.18', '0.57',
                          '0.93', '0.25', '0.71', '0.79', '0.63', '0.88',
                          '0.39', '0.91', '0.32', '0.83', '0.54', '0.95',
                          '0.20', '0.60', '0.91', '0.30', '0.80', '0.60'))
})

test_that('chain: basics', {
  result <- iter.chain(c(1, 2, 3), c(4, 5))
  # expect_equal(result, c(1, 2, 3, 4, 5))
})

test_that('compress: basics', {
  result <- iter.compress(c(1, 2, 3), c(T, F, T))
  expect_equal(result, c(1, 3))
  
  result <- iter.compress(list(1, 2, 3), c(T, F, T))
  expect_equal(result, list(1, 3))
  
  result <- iter.compress(list(x=1, y=2, z=3), c(T, F, T))
  expect_equal(result, list(x=1, z=3))
})

test_that('dropwhile: basics', {
  result <- iter.dropwhile(function(x) { x < 5 }, c(1, 4, 6, 4, 1))
  expect_equal(result, c(6, 4, 1))
})

test_that('takewhile: basics', {
  result <- iter.takewhile(function(x) { x < 5 }, c(1, 4, 6, 4, 1))
  expect_equal(result, c(1, 4))
})

test_that('filterfalse: basics', {
  result <- iter.filterfalse(function(x) { x %% 2 }, seq(0, 9))
  expect_equal(result, c(0, 2, 4, 6, 8))
})

test_that('islice: basics', {
  single <- 'ABCDEFG'
  vec <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')
  expect_equal(iter.islice(single, 2), c('A', 'B'))
  expect_equal(iter.islice(vec, 2), c('A', 'B'))
  
  expect_equal(iter.islice(single, 3, 4), c('C', 'D'))
  expect_equal(iter.islice(vec, 3, 4), c('C', 'D'))
  
  expect_equal(iter.islice(single, 3, NULL), c('C', 'D', 'E', 'F', 'G'))
  expect_equal(iter.islice(vec, 3, NULL), c('C', 'D', 'E', 'F', 'G'))
  
  expect_equal(iter.islice(single, 1, NULL, 2), c('A', 'C', 'E', 'G'))
  expect_equal(iter.islice(vec, 1, NULL, 2), c('A', 'C', 'E', 'G'))
})

test_that('starmap: basics', {
  result <- iter.starmap(sqrt, list(9, 4, 16))
  expect_equal(result, list(3, 2, 4))

  f <- function(x, y, z) { x + y + z }
  result <- iter.starmap(f, list(c(1, 2, 3), c(2, 3, 4)))
  expect_equal(result, list(6, 9))
})
