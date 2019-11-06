library(testthat) 
library(here)

# FIXME: NaNs are produced when calculating sqrt(x^2 - y^2 + 1) in some cases for large beta angles (for beta >= 40 when +30 > phi > -30).
#        Where? In sunrise_for_inclined_surface_oriented_east and sunrise_for_inclined_surface_oriented_west:
#       x ^2 - y^2 + 1 results in a negative number thus outputting NaN when calling sqrt().        
sunrise = dget(here("utils", "sunrise.R"))

test_that("Sunrise - Exhaustive Warnings Test.", {
  Ls_issues = c()
  phi_issues = c()
  beta_issues = c()
  gamma_issues = c()
  
  for(Ls in seq(0, 360, 80)){
    for(p in seq(-30, 30, 10)){
      for(b in seq(0, 80, 5)){
        for(gc in seq(-180, 180, 5)){
          
          tryCatch(
            {
              T_sr = NULL
              T_sr_i = NULL
              
              T_sr = sunrise(Ls=Ls, phi=p, unit=3, beta=NULL, gamma_c=NULL)
              T_sr_i = sunrise(Ls=Ls, phi=p, unit=3, beta=b, gamma_c=gc)
            },
            error=function(cond) {
              # cat("\n============================================================")
              # cat(paste("\n\nError:", cond))
              # cat(paste("T_sr: ", T_sr, ",    T_sr_i:", T_sr_i))   
              # cat(paste("\nLs=", Ls, "phi=, ", p, ", beta=", b, ", gamma_c=", gc, "\n", sep=""))
            },
            warning=function(cond) {
              # cat("\n============================================================")
              # cat(paste("\n\nWarning:", cond))
              # cat(paste("T_sr: ", T_sr, ",    T_sr_i:", T_sr_i))   
              # cat(paste("\nLs=", Ls, ", phi=", p, ", beta=", b, ", gamma_c=", gc, "\n", sep=""))
              
              Ls_issues <<- c(Ls_issues, Ls)
              phi_issues <<- c(phi_issues, p)
              beta_issues <<- c(beta_issues, b)
              gamma_issues <<- c(gamma_issues, gc)
            }
          )  
        }
      }
    }
  }
  
  # Expect no warnings triggered.
  expect_length(Ls_issues, 0)
  
  if(length(Ls_issues) != 0){
    cat("\nProblematic variables:\n")
    #cat(paste("\n\tLs:", paste(unique(Ls_issues), collapse=", ")))
    cat(paste("\n\tphi:", paste(unique(phi_issues), collapse=", ")))
    cat(paste("\n\tbeta:", paste(unique(beta_issues), collapse=", ")))
    cat(paste("\n\tgamma_c:", paste(unique(gamma_issues), collapse=", ")))
  }
})