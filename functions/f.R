# Equation 20. The analytical expression of the normalized net flux function.
# Uses a lookup table to retrieve coefficients.
#
# TODO: Merge f_89.R, f_90.R, and f.R into a single parameterized function.
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
#   al    - Albedo (from 0 to 1 but can only validate against 0.1 and 0.4 in literature).
function(Z, tau, al){
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