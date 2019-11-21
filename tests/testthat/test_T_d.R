
tolerance = 0

# FIXME: Test against values in Table IV - Update 1990
# FIXME: Test against results from sunset - sunrise.
test_that("T_d", {
  for(Ls in 0){
    for(phi in seq(0, 80, 10)){
      Td = T_d(Ls=Ls, phi=phi)
      expect_equal(Td, 12, tolerance=tolerance, scale=1)
    }
  }
})
