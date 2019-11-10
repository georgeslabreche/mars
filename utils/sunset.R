# Get the moment in which the sunset occurs.
# FIXME: 
#   1. Basd logic when gamma_c = 0 and Beta > 0.

library(here)

# Equation 8 (1993): Sunrise hour angle.
source(here("utils", "sunrise.R"))

# Equation 7 (1990): The declination angle.
source(here("utils", "declination.R"))

# (31) in (1993).
# Angles in rad.
# TODO: Make this a hidden function.
# Doesn't work for all cases, see Ls = 300, phi = 20, and beta = 45.
sunset_for_inclined_surface_oriented_equator = function(phi, beta, delta){
  omega_rad = acos(-tan(delta) * tan(phi-beta))
  
  return(omega_rad)
}

# (16) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
sunset_for_inclined_surface_oriented_east = function(Ls, phi, beta, gamma_c, delta){
  omega_rad_1 = -sunrise(Ls=Ls*180/pi, phi=phi*180/pi, unit=1, beta=NULL, gamma_c=NULL)
  
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
  
  a = -x*y + sqrt(x^2 - y^2 + 1)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  omega_rad = min(omega_rad_1, omega_rad_2)
  
  return(omega_rad)
}

# (18) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
sunset_for_inclined_surface_oriented_west = function(Ls, phi, beta, gamma_c, delta){
  omega_rad_1 = -sunrise(Ls=Ls*180/pi, phi=phi*180/pi, unit=1, beta=NULL, gamma_c=NULL)
  
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
  
  a = -x*y - sqrt(x^2 - y^2 + 1)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  omega_rad = min(omega_rad_1, omega_rad_2)
  
  return(omega_rad)
}

# Angles in rad.
# TODO: Make this a hidden function.
sunset_for_inclined_surface = function(Ls, phi, beta, gamma_c, delta){
  
  # FIXME: See function - sunset_for_inclined_surface_oriented_equator.
  if(gamma_c == 0){
    gamma_c = 1e-10
  }
  
  # Inclination angle is 0 degrees, this is equivalent to a horizontal surface.
  if(beta == 0){
    omega_rad = -sunrise(Ls=Ls, phi=phi, unit=1)
    
  }else if(gamma_c %in% c(0, -180) && phi > 0){ # Inclined surface is oriented South from the northern hemisphere (i.e. towards the equator).
    omega_rad = sunset_for_inclined_surface_oriented_equator(phi=phi, beta=beta, delta=delta)
    
  }else if(gamma_c == 180 && phi < 0){ #  Inclined surface is oriented North from the southern hemisphere (i.e. towards the equator).
    omega_rad = sunset_for_inclined_surface_oriented_equator(phi=phi, beta=beta, delta=delta)
    
  }else if(gamma_c < 0){ # Inclined surface is oriented towards the East.
    omega_rad = sunset_for_inclined_surface_oriented_east(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else if(gamma_c > 0){ # Incline surface is oriented towards the West.
    omega_rad = sunset_for_inclined_surface_oriented_west(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else{
    # This should not happen.
    stop("An unknown error has occurred, could not determine sunset hour angle for an incline surface.")
  } 
  
  return(omega_rad)
}

# The function.
#   Ls    - Areocentric longitude.
#   phi   - Planetary latitude.
#   unit  - Unit to return:
#           - 1 for radians.
#           - 2 for degrees.
#           - 3 for solar hour.
sunset = function(Ls, phi, unit=1, beta=NULL, gamma_c=NULL){
  
  omega_rad = NULL
  
  # Equation 7 (1990): Declination angle [rad].
  delta_rad = declination(Ls)
  
  if((is.null(beta) && is.null(gamma_c))){
    omega_rad = -sunrise(Ls=Ls, phi=phi, unit=1)
    
  }else if(!is.null(beta) && !is.null(gamma_c)){
    if(gamma_c > 180 || gamma_c < -180){
      stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
    }
    
    omega_rad = sunset_for_inclined_surface(Ls=Ls*pi/180, phi=phi*pi/180, beta=beta*pi/180, gamma_c=gamma_c*pi/180, delta=delta_rad)
    
  }else{
    stop("Invalid argument values. Both beta and gamma_c should either be NULL or not NULL.")
  }
  
  if(is.null(omega_rad)){
    stop("An unknown error has occurred. The sunrise hour angle has not been set.")
  }
  
  # Get the hour angle in degrees.
  omega_deg = omega_rad * 180/pi
  
  # Equation 8 (1990): Hour angle. Determine the Surise and Sunset times [h].
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  T_s = (omega_deg + 180) / 15
  
  if(unit == 1){
    return(omega_rad)
    
  }else if(unit == 2){
    return(omega_deg)
    
  }else if(unit == 3){
    return(T_s)
    
  }else{
    # This should not happen.
    stop("An unknown error has occurred.")
  }  
}

#######################################
# Testing code. To eventually remove. #
#######################################
# Ls = 100
# phi = 2
# 
# for(b in seq(0, 35, 5)){
#  for(g in seq(-180, 180, 5)){
# # for(b in c(35)){
# #   for(g in c(30)){
#     omega_ss = sunset(Ls=Ls, phi=phi, unit=1, beta=NULL, gamma_c=NULL)
#     omega_ss_i = sunset(Ls=Ls, phi=phi, unit=1, beta=b, gamma_c=g)
#     
#     T_ss = (omega_ss*180/pi + 180) / 15
#     T_ss_i = (omega_ss_i*180/pi + 180) / 15
#     
#     if(T_ss != T_ss_i){
#       cat("\n\n=====================================")
#       cat(paste("\n\nbeta:", b, ", gamma_c:", g))
#       cat(paste("\nT_ss:", T_ss, ", T_ss_i:", T_ss_i))
#     }
#   }
# }


#######################################
# Testing code. To eventually remove. #
#######################################

# Ls = 300
# p = 20
# b = 45
# d = declination(Ls)
# 
# orientation_angles = seq(-180, 180, 1)
# sunset_hours = c()
# 
# for(g in orientation_angles){
#   sr = sunset(Ls, p, unit=3, beta=b, gamma_c=g)
#   sunset_hours = c(sunset_hours, sr)
# }
# 
# 
# dev.new()
# plot(orientation_angles, sunset_hours,
#      ylab="<-- EARLIER      |      LATER -->",
#      xlab="<-- EAST      |      WEST -->")

