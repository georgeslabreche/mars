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

if(!exists("k0_coeffs")){
  k0_coeffs = f_build_coefficients_df(k=0)
}

if(!exists("k1_coeffs")){
  k1_coeffs = f_build_coefficients_df(k=1)
}

# build a dataframe representation of Table III referenced in Appelbaum, Joseph & Flood, Dennis. (1990):
if(!exists("nnff_0p1_89")){
  nnff_0p1_89 = f_build_df(al=0.1, pub_year=1989)
}

# Build dataframe representation of Table III and IV referenced in Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990.
if(!exists("nnff_0p1_90")){
  nnff_0p1_90 = f_build_df(al=0.1, pub_year=1990)
}

if(!exists("nnff_0p4_90")){
  nnff_0p4_90 = f_build_df(al=0.4, pub_year=1990)
}

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
  
  nnff_df = NULL
  
  if(al == 0.1){
    nnff_df = nnff_0p1_89
    
  }else{
    stop("The albedo can only be 0.1 when using f_89 table lookup.")
  }
  
  return(
    unlist( # Unlist in case a sequence of Zs are given instead of a single value (i.e. in the case of integrations).
      nnff_df[nnff_df$tau == toString(tau), paste("X", Z, sep="")],
      use.names=FALSE
    )
  )
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
  
  nnff_df = NULL
  
  if(al == 0.1){
    nnff_df = nnff_0p1_90
    
  }else if(al == 0.4){
    nnff_df = nnff_0p4_90
    
  }else{
    stop("The albedo can only be 0.1 or 0.4 when using f_90 table lookup.")
  }
  
  return(
    unlist( # Unlist in case a sequence of Zs are given instead of a single value (i.e. in the case of integrations).
      nnff_df[nnff_df$tau == toString(tau), paste("X", Z, sep="")],
      use.names=FALSE
    )
  )
}

# Equation 20. The analytical expression of the normalized net flux function.
#
# From Appelbaum, Joseph & Flood, Dennis (1990) - Update 1990:
#   The mean error is about 0.7 percent for the full range. For zenith angles up to 40° the error is much smaller.
#   The largest error is for zenith angle of 80° and 85° and for τ greater than 5. The maximum error is about 7 percent.
#   At these large angles and opacities, the error has a minor effect on the calculated daily insolations.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
#   https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990
#
f_analytical = function(Z, tau, al=0.1){
  # Check for and warn against parameters that would result in lagest errors (max. 7%).
  if(tau > 5){
    warning(paste("Large error encountered with τ = ", tau, " greater than 5 (maximum error is 7%). ", 
                  "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.",
                  sep="")
    )
  }
  

  # Use ifelse in case this function is being invoked from an integration in which case Z can be a vector instead of a scalar.
  # If Z is a scalar and we use if() then the following issue will occur: "the condition has length > 1 and only the first element will be used."
  warning_msg = ifelse(Z >= 80, paste("Possibly large error encountered with Z = ", Z, "° (maximum error is 7% for Z = 80° or Z = 85°). ",
                                      "Consider using the f_89 and f_90 table lookup implementation of the normalized net flux function instead of its analytical expression.\n",
                                      sep=""), "")
  
  # handle warning message.
  for(w_msg in warning_msg){
    if(w_msg != ""){
      warning(w_msg)
    }
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
f = function(Z, tau, al=0.1, pub_year=NULL){
  net_flux = NULL
  
  if(is.null(pub_year)){
    net_flux = f_analytical(Z, tau, al)
    
  }else if(pub_year == 1989){
    net_flux = f_89(Z, tau, al)
    
  }else if(pub_year == 1990){
    net_flux = f_90(Z, tau, al)
    
  }else{
    stop("Usupported publication year, should either be 1989 for the original pulication or 1990 for its 1990 update")
  }
  
  # Check if given Z results in NULL net flux.
  if(is.null(net_flux)){
    stop(paste("Sun zenith angle Z = ", Z ,"° is not available in the net flux look-up table. Consider using the analytical function instead.", sep=""))
    
  }
  
  # Check if given tau results in NA net flux.
  # Use ifelse in case this function is being invoked from an integration in which case net_flux can be a vector instead of a scalar.
  isNAs = ifelse(is.na(net_flux), TRUE, FALSE)
  for(isNA in isNAs){
    if(isTRUE(isNA)){
      stop(paste("Optical depth tau factor τ = ", tau," is not available in the net flux look-up table. Consider using the analytical function instead.", sep=""))
    }
  }
  
  return(net_flux) 
}