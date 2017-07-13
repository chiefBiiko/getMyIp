# test getMyIp

testthat::context('getMyIp')

testthat::test_that('privateV4', {
  testthat::expect_true(greplV4(privateV4()))
})

testthat::test_that('publicV4', {
  testthat::expect_true(greplV4(publicV4()))
})

testthat::test_that('listV4', {
  testthat::expect_type(listV4(), 'list')
  testthat::expect_length(listV4(), 2L)
  testthat::expect_true(all(sapply(listV4(), greplV4)))
})

testthat::test_that('private is not localhost', {
  testthat::expect_false(greplLocalhost(privateV4()))
})
