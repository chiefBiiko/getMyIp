# test getMyIp

testthat::context('getMyIp')

testthat::test_that('privateV4', {
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', privateV4()), TRUE)
  
})

testthat::test_that('publicV4', {
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', publicV4()), TRUE)
  
})

testthat::test_that('listV4', {
  
  testthat::expect_type(listV4(), 'list')
  
})

testthat::test_that('privateV4 is not localhost', {
  
  testthat::expect_identical(grepl('127\\.0\\.0\\.1', privateV4()), FALSE)

})