# test getMyIp

testthat::context('getMyIp')

testthat::test_that('private', {
  testthat::expect_true(greplV4(privateIp()))
  testthat::expect_true(greplV6(privateIp(version=6L)))
})

testthat::test_that('public', {
  testthat::expect_true(greplV4(publicIp(4L)) || greplV6(publicIp(6L)))
})

testthat::test_that('listV4', {
  testthat::expect_type(listIps(), 'list')
  testthat::expect_length(listIps(), 2L)
})

testthat::test_that('private is not localhost', {
  testthat::expect_false(greplLocalhost(privateIp()))
  testthat::expect_false(greplLocalhost(privateIp(version=6L)))
})
