# Equation 20. The analytical expression of the normalized net flux function.
#
#
# TODO: 
#   - Merge f_89.R, f_90.R, and f.R into a single parameterized function.
#   - Mars surface albedo value can be determined as a function of Longitude and Latitude (Table I.)
#
# From Appelbaum, Joseph & Flood, Dennis (1990) - Update 1990:
#   The mean error is about 0.7 percent for the full range. For zenith angles up to 40° the error is much smaller.
#   The largest error is for zenith angle of 80° and 85° and for τ greater than 5. The maximum error is about 7 percent.
#   At these large angles and opacities, the error has a minor effect on the calculated daily insolations.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
# https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990

f_build_coefficients_df = dget("functions/f_build_coefficients_df.R")

k0_coeffs = f_build_coefficients_df(k=0)
k1_coeffs = f_build_coefficients_df(k=1)

# The coefficient lookup function.
# See Table IV - Normalized Net Flux Function Coefficients in Appelbaum, Joseph & Flood, Dennis (1990) Update 1990. 
p = function(i, j, k){
  if(k==0){
    coefficient = k0_coeffs[j+1, paste("X", i, sep="")]
  }else if(k==1){
    coefficient = k1_coeffs[j+1, paste("X", i, sep="")]
  }else{
    stop("Unsupported k value.")
  }
  
  return(coefficient)
}

# The net flux function.
#   Z     - Zenith angle [deg].
#   tau   - Optical depth tau factor.
#   al    - Albedo (ranges from 0.1 to 0.4).
function(Z, tau, al){
  
  # Check for and warn against parameters that would result in lagest errors (max. 7%).
  if(tau > 5){
    warning(paste("Large error encountered with τ=", tau, " greater than 5 (maximum error is 7%). ", 
                  "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.",
                  sep="")
            )
  }
  
  if(Z == 80 || Z == 85){
    warning(paste("Large error encountered with Z=", Z, "° (maximum error is 7%). ", 
                  "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.",
                  sep="")
            )
  }
  
  psum = 0
  for(i in seq(0,5,1)){
    for(j in seq(0,5,1)){
      for(k in seq(0,1,1)){
        psum = psum + p(i, j, k) * tau^i * (Z/100)^j * al^k
      }
    }
  }
   
  psum * (1-al)
}