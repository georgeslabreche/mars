# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977

# FIXME: Update this function so that it figures out if its a polar night or day.

#' Title
#'
#' @param Ls 
#' @param phi 
#' @param Ts 
#' @param z 
#' @param tau 
#' @param beta 
#' @param gamma_c
#'
#' @return
#' @export
G_bi = function(Ls, phi, Ts, z=Z(Ls=Ls, phi=phi, Ts=Ts), tau, beta, gamma_c){
  
  if(gamma_c > 180 || gamma_c < -180){
    stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
  }
  
  # Equation 7 (1990): Declination angle [rad].
  delta = declination(Ls)
  
  # Equation 8 (1990): Hour angle [deg].
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mars hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega_deg = 15 * Ts - 180
  
  # Set to omega to exactly zero when omega near zero because sometimes it is essentially zero with values like -2.8421709430404e-14 deg.
  zero = function(x){
    return(
      ifelse(x > -1e-5 && x < 1e-5, 0, x)
    )
  }
  
  # Apply the function that will set omega to zero if it is close to zero.
  omega_deg = sapply(omega_deg, zero)
  
  # Equation 6 (1993): Solar Azimuth Angle [deg]
  i = sin(phi*pi/180) * cos(delta) * cos(omega_deg*pi/180)
  j = cos(phi*pi/180) * sin(delta)
  k = sin(z*pi/180)
  
  # From (32) in (1993): It is solar noon when omega is 0°. This translates to gamma_s = 0°.
  # The following operation will result in a value very close to 1 but not exactly 1 if it is
  # solar noon, i.e. when omega is 0 deg. When op = 1 -> acos(op) = 0, i.e. (32) in (1993).
  op = (i - j) / k
  
  # The following function is to make sure that op is:
  #   - exactly 1 when omega is 0° (or else NaN is produced when calculating gamma_s)
  #   - exactly 0 when omega is 90°.
  adjust = function(x){
    return(
      ifelse(x > -1e-5 && x < 1e-5, 0,
             ifelse(x > 0,
                    ifelse(x > 1 && x < 1+1e-5, 1, x),  # If x is positive.
                    ifelse(x < -1 && x > -1-1e-5, 1, x))) # If x is negative.
    )
  }

  # Apply the function.
  op = sapply(op, adjust)
  
  # Calculate gamma_s.
  gamma_s = acos(op) * 180/pi # [deg]
    
  # Alternatively, Equation 7 (1993): Solar Azimuth Angle [deg]
  # x = sin(phi*pi/180) * cos(z*pi/180) - sin(delta)
  # y = cos(phi*pi/180) * sin(z*pi/180)
  # 
  # gamma_s = acos(x / y) * 180/pi # [deg]

  # Sun Angle of Incidence [rad].
  sun_angle_of_incidence = function(){
    teta = NULL
    
    # (23) in (1993) Vertical surface.
    if(beta == 90){
      i = cos(gamma_s * pi/180) * cos(gamma_c * pi/180)
      #TODO:  Double check this, why wouldn't the paper use squared instead of multiply the by the same value? 
      #       Check with (24) in (1993). 
      j = sin(gamma_s * pi/180) * sin(gamma_s * pi/180) 
      teta = acos(sin(z * pi/180) * (i + j))# [rad]
      
    }else{
      # (13) in (1993): Inclined surface.
      # (27) in (1993): Surface facing the equator.
      i = cos(beta * pi/180) * cos(z * pi/180)
      j = sin(beta * pi/180) * sin(z * pi/180) * cos((gamma_s - gamma_c) * pi/180) # Does not matter when beta = 0 because it leads to j = 0.
      teta = acos(i + j) # [rad]
    }
    
    return(teta)
  }
  
  # Sun Angle of Incidence [rad] on an inclined surface.
  teta = sun_angle_of_incidence()
  
  # Calculate Gbi.
  Gbi = G_b(Ls=Ls, z=z, tau=tau) * cos(teta)
  
  return(Gbi)
}