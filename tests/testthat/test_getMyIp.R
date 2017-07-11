# test getMyIp

testthat::context('getMyIp')

testthat::test_that('privateV4', {
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', privateV4()),
                             TRUE)
  
})

testthat::test_that('publicV4', {
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', publicV4()),
                             TRUE)
  
})

testthat::test_that('listV4', {
  
  testthat::expect_type(listV4(), 'list')
  
})