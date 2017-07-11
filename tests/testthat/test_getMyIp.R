# test getMyIp

testthat::context('getMyIp')

testthat::test_that('match ipv4', {
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', privateV4()),
                             TRUE)
  
  testthat::expect_identical(grepl('^(\\d+\\.){3}\\d+$', publicV4()),
                             TRUE)
  
  testthat::expect_type(listV4(), 
                        'list')
  
})