context('test base functions')

test_that('.preprocess', {
  result <- .preprocess(c(1, 2, 3, 4, 5))
  expect_equal(result, c(1, 2, 3, 4, 5))

  result <- .preprocess(1)
  expect_equal(result, c(1))
  
  result <- .preprocess(c('AAA'))
  expect_equal(result, c('A', 'A', 'A'))
  
  result <- .preprocess(c('A', 'B'))
  expect_equal(result, c('A', 'B'))
  
  result <- .preprocess(list(1, 2, 3, 4, 5))
  expect_equal(result, list(1, 2, 3, 4, 5))
  
  result <- .preprocess(list('AAA'))
  expect_equal(result, list('AAA'))
  
  result <- .preprocess(list('A', 'B'))
  expect_equal(result, list('A', 'B'))
})