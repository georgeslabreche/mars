# Equation 20 (1990): The analytical expression of the normalized net flux function.
#
# From Appelbaum, Joseph & Flood, Dennis (1990) - Update 1990:
#   The mean error is about 0.7 percent for the full range. For zenith angles up to 40° the error is much smaller.
#   The largest error is for zenith angle of 80° and 85° and for τ greater than 5. The maximum error is about 7 percent.
#   At these large angles and opacities, the error has a minor effect on the calculated daily insolations.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
#   https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990

library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

f_build_coefficients_df = dget(here("functions", "f_build_coefficients_df.R"))

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

# The normalized net flux function's lookup table lookup.
#
# Uses a looktup Table III published in "Solar radiation on Mars"
#
# From Appelbaum, Joseph & Flood, Dennis. (1990):
#   The net solar flux integrated over the solar spectrum on the Martian
#   surface was calculated by Pollack basd on multiple wavelength and
#   multiple scattering of the solar radiation. Derived data from this
#   calculation are shown Table III by the normalized net flux function
#   f(Z, tau) where the parameters are the zenith angle Z and the optical
#   depth tau. This table pertains to an albedo of 0.1 but can be used
#   for higher albedo values to a first approximation.
#
# Based on the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars
#
#   Z     - Zenith angle [deg].
#   tau   - Optical depth tau factor.
#   al    - Albedo. Can only be 0.1 for 1989 data.
f_89 = function(Z, tau, al=0.1){
  # We build a dataframe representation of Table III referenced in Appelbaum, Joseph & Flood, Dennis. (1990):
  nnff = f_build_df(al=al, pub_year=1989)
  return(nnff[sprintf("%1.2f", tau), paste("X", Z, sep="")])
}

# The normalized net flux function's lookup table lookup.
#
# Uses the looktup table published in "Solar radiation on Mars: Update 1990."
#   - Table III (a) and (b) for an albedo of 0.1.
#   - Table IV  (a) and (b) for an albedo of 0.4 
#
# Based on the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
#   https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990
# 
#   Z     - Zenith angle [deg].
#   tau   - Optical depth tau factor.
#   al    - Albedo. Can be 0.1 or 0.4 for 1990 data.
f_90 = function(Z, tau, al=0.1){
  # We build a dataframe representation of Table III and IV referenced in Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990.
  nnff = f_build_df(al=al, pub_year=1990)
  return(nnff[sprintf("%1.2f", tau), paste("X", Z, sep="")])
}

# Equation 20. The analytical expression of the normalized net flux function.
#
# TODO: 
#   - Mars surface albedo value can be determined as a function of Longitude and Latitude (Table I.)
#
# From Appelbaum, Joseph & Flood, Dennis (1990) - Update 1990:
#   The mean error is about 0.7 percent for the full range. For zenith angles up to 40° the error is much smaller.
#   The largest error is for zenith angle of 80° and 85° and for τ greater than 5. The maximum error is about 7 percent.
#   At these large angles and opacities, the error has a minor effect on the calculated daily insolations.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
#   https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990
f_analytical = function(Z, tau, al=0.1){
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
  
  return(psum * (1-al))
}


# The net flux function.
#   Z     - Zenith angle [deg].
#   tau   - Optical depth tau factor.
#   al    - Albedo (ranges from 0.1 to 0.4).
function(Z, tau, al=0.1, pub_year=NULL){

  if(is.null(pub_year)){
    f_analytical(Z, tau, al)
    
  }else if(pub_year == 1989){
    f_89(Z, tau, al)
    
  }else if(pub_year == 1990){
    f_90(Z, tau, al)
    
  }else{
    stop("Usupported publication year, should either be 1989 for the original pulication or 1990 for its 1990 update")
  }
}