context("Sunrise")

# For a given planetary latitude, the sunrise time is expected to be the same throughout the entire year
# for all orientations when on a horizontal surface.
test_that("Sunrise time on an horizontal surface facing 'all' orientations.", {

  sunrise_hours <<- c()

  for(Ls in seq(0, 360, 20)){
    for(phi in seq(-90, 90, 10)){
      for(gamma_c in seq(-180, 180, 10)){
        sr = sunrise(Ls=Ls, phi=phi, beta=0, gamma_c=gamma_c, unit=3)
        sunrise_hours <<- c(sunrise_hours, sr)
      }

      expect_length(unique(sunrise_hours), 1)
      sunrise_hours <<- c()
    }
  }
})

#TODO: Test cases:
#   1. At the equator, when phi = 0. omega_sr = pi/2.
#   2. At the equinox, when delta = 0. omega_sr = pi/2.
test_that("Sunrise time on an inclined surface facing the equator.", {

  # Tolerance
  tolerance = 0

  # Sunrise data to test against. For beta = phi.
  expected_sunrise_times = read.csv("data/sunrise_beta_equals_phi_table_i_1993.csv")

  # Rename rows labels (Ls).
  rownames(expected_sunrise_times) = sprintf("%i", expected_sunrise_times[,1])
  expected_sunrise_times = expected_sunrise_times[-c(1)]

  # Rename column labels (beta/phi). Use 'm' for minus/negative and 'p' for plus/positive
  colnames(expected_sunrise_times) = gsub("X\\.", "m", colnames(expected_sunrise_times))
  colnames(expected_sunrise_times) = gsub("X", "p", colnames(expected_sunrise_times))
  colnames(expected_sunrise_times) = gsub("p0", "0", colnames(expected_sunrise_times))

  # Functo to fetch sunrise time from expected sunrise table.
  get_expected_sunrise_time = function(Ls, beta_phi){

    # Turn Ls value into row label value.
    Ls = toString(Ls)

    # Turn beta/phi angle numeric value into column label string value.
    if(beta_phi == 0){
      beta_phi = toString(beta_phi)

    }else if(beta_phi > 0){
      beta_phi = paste("p", beta_phi, sep="")

    }else if(beta_phi < 0){
      beta_phi = paste("m", abs(beta_phi), sep="")
    }

    # Get sunrise hour.
    sunrise = expected_sunrise_times[Ls, beta_phi]

    # For expected sunrise times, the 6.00 and 12.00 times for high altitudes
    # indicate polar days and polar nights respectively (see 1993).
    if(beta_phi >= 70){
      if(sunrise %in% c(6, 12)){
        return(NA)
      }
    }

    # Return sunrise hour.
    return(sunrise)
  }

  # Assert equals test against all values in the expected sunrise table.
  for(Ls in seq(0, 360, 5)){

    # The expected data was obtained with Phi = Beta. See Table I in (1993).
    for(phi_beta in seq(-90, 90, 10)){

      # Make sure that orientation is towards the equator because that is what we are testing.
      # If on a flat horizontal surface, i.e. beta = 0, then it doesn't matter what the orientation is (i.e. gamma_c).
      if(phi_beta > 0){
        # Located in the northern hemisphere: orient towards the south.
        gamma_c_vect = c(0)

      }else if(phi_beta < 0){
        # Located in the southern hemisphere: orient towards the north
        gamma_c_vect = c(-180, 180)
      }

      for(gamma_c in gamma_c_vect){
        # Get expected sunrise time from test table.
        sunrise_expected = get_expected_sunrise_time(Ls, phi_beta)

        # Calculate sunrise time.
        sunrise_calculated = sunrise(Ls=Ls, phi=phi_beta, beta=phi_beta, gamma_c=gamma_c, unit=3)
        sunrise_calculated = ifelse(is.na(sunrise_calculated), NA, round(sunrise_calculated, 2))

        # Due to floating point precision, days that are calculated to be just on the edge of polar day may not match with
        # expected results which state that they are exactly on polar days.
        # This workaround addresses that.
        # FIXME: Why is this not an issue with polar nights?
        if(is.na(sunrise_expected) && !is.na(sunrise_calculated) && round(sunrise_calculated, 4) == 6){
          sunrise_expected = 6
        }

        #print(paste("Ls:", Ls, "- phi/beta:", phi_beta, "- gamma_c:", gamma_c, "- sunrise_c:", sunrise_calculated, "- sunrise_e:", sunrise_expected))
        expect_equal(sunrise_calculated, sunrise_expected, tolerance=tolerance, scale=1)
      }
    }
  }
})

# FIXME: NaNs are produced when calculating sqrt(x^2 - y^2 + 1) in some cases for large beta angles (for beta >= 40 when +30 > phi > -30).
#        Where? In sunrise_for_inclined_surface_oriented_east and sunrise_for_inclined_surface_oriented_west:
#       x ^2 - y^2 + 1 results in a negative number thus outputting NaN when calling sqrt().        
# test_that("Sunrise on an inclined surface - Exhaustive Warnings Test.", {
#   Ls_issues = c()
#   phi_issues = c()
#   beta_issues = c()
#   gamma_issues = c()
# 
#   # for(Ls in seq(0, 360, 80)){
#   #   for(p in seq(-30, 30, 10)){
#   #     for(b in seq(0, 80, 5)){
#   #       for(gc in seq(-180, 180, 5)){
#           
#   for(Ls in c(80)){
#     for(p in seq(30, 30, 10)){
#       for(b in seq(80, 80, 5)){
#         for(gc in seq(-180, 180, 5)){
# 
#           tryCatch(
#             {
#               Tsr = NULL
#               Tsr_i = NULL
# 
#               Tsr = sunrise(Ls=Ls, phi=p, beta=NULL, gamma_c=NULL, unit=3)
#               Tsr_i = sunrise(Ls=Ls, phi=p, beta=b, gamma_c=gc, unit=3)
#             },
#             error=function(cond) {
#               # cat("\n============================================================")
#               # cat(paste("\n\nError:", cond))
#               # cat(paste("Tsr: ", Tsr, ",    Tsr_i:", Tsr_i))
#               # cat(paste("\nLs=", Ls, "phi=, ", p, ", beta=", b, ", gamma_c=", gc, "\n", sep=""))
#             },
#             warning=function(cond) {
#               # cat("\n============================================================")
#               # cat(paste("\n\nWarning:", cond))
#               # cat(paste("Tsr: ", Tsr, ",    Tsr_i:", Tsr_i))
#               # cat(paste("\nLs=", Ls, ", phi=", p, ", beta=", b, ", gamma_c=", gc, "\n", sep=""))
#               
#               # Enter here when NaNs produced from sqrt(x^2 - y^2 + 1).
# 
#               Ls_issues <<- c(Ls_issues, Ls)
#               phi_issues <<- c(phi_issues, p)
#               beta_issues <<- c(beta_issues, b)
#               gamma_issues <<- c(gamma_issues, gc)
#             }
#           )
#         }
#       }
#     }
#   }
# 
#   # Expect no warnings triggered.
#   expect_length(Ls_issues, 0)
# 
#   if(length(Ls_issues) != 0){
#     cat("\nProblematic variables:\n")
#     cat(paste("\n\tLs:", paste(unique(Ls_issues), collapse=", ")))
#     cat(paste("\n\tphi:", paste(unique(phi_issues), collapse=", ")))
#     cat(paste("\n\tbeta:", paste(unique(beta_issues), collapse=", ")))
#     cat(paste("\n\tgamma_c:", paste(unique(gamma_issues), collapse=", ")))
#   }
# })