# Get the moment in which the sunrise occurs.
# FIXME: 
#   1. Basd logic when gamma_c = 0 and Beta > 0.
#   2. For vertical surface (when Beta=90)

library(here)

# Equation 7 (1990): The declination angle.
source(here("utils", "declination.R"))

# (19) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
# FIXME: x is Inf when gamma_c = 0.
x_for_inclined_surface = function(phi, beta, gamma_c){
  
  # FIXME: When gammac_c is 0 then a divide by 0 is introduced because sin(0) = tan(0) = 0.
  a = cos(phi) / (sin(gamma_c) * tan(beta)) 
  b = sin(phi) / tan(gamma_c)
  
  x = a + b
  
  return(x)
}

# (20) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
# FIXME: x is NaN when gamma_c = 0.
y_for_inclined_surface = function(phi, beta, gamma_c, delta){
  
  # FIXME: When gammac_c is 0 then a divide by 0 is introduced because sin(0) = tan(0) = 0.
  a = sin(phi) / (sin(gamma_c) * tan(beta))
  b = cos(phi) / tan(gamma_c)
  
  y = tan(delta) * (a - b)
  
  return(y)
}

# (15) from (1993)
# Angles in rad.
# TODO: Make this a hidden function.
sunrise_for_inclined_surface_oriented_east = function(phi, beta, gamma_c, delta){
  omega_rad_1 = sunrise_for_horizontal_surface(phi=phi, delta=delta, 1)
  #print(paste("omega_rad_1", omega_rad_1))
  
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
  
  # FIXME: In some cases there are negative values inside the sqrt()
  #        e.g. Ls=13, phi=22.3, beta=65, gamma_c=-175.
  a = -x*y - sqrt(x^2 - y^2 + 1)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  omega_rad = -min(abs(omega_rad_1), omega_rad_2)
  
  return(omega_rad)
}

# (17) from (1993)
# Angles in rad.
# TODO: Make this a hidden function.
sunrise_for_inclined_surface_oriented_west = function(phi, beta, gamma_c, delta){
  omega_rad_1 = sunrise_for_horizontal_surface(phi=phi, delta=delta, 1)
  
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
  
  # FIXME: In some cases there are negative values inside the sqrt()
  a = -x*y + sqrt(x^2 - y^2 + 1)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  omega_rad = -min(abs(omega_rad_1), omega_rad_2)
  
  return(omega_rad)
}

# Angles in rad.
# TODO: Make this a hidden function.
sunrise_for_inclined_surface = function(phi, beta, gamma_c, delta){
  
  # FIXME: When gammac_c is 0 then a divide by 0 is introduced because sin(0) = tan(0) = 0.
  if(gamma_c == 0){
    gamma_c = 1e-10
  }

  # Inclination angle is 0 degrees, this is equivalent to a horizontal surface.
  if(beta == 0){
    omega_rad = sunrise_for_horizontal_surface(phi=phi, delta=delta, unit=unit)
    
  }else if(gamma_c < 0){ # Inclined surface is oriented towards the East.
    omega_rad = sunrise_for_inclined_surface_oriented_east(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else if(gamma_c > 0){ # Incline surface is oriented towards the West.
    omega_rad = sunrise_for_inclined_surface_oriented_west(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else{
    # This should not happen.
    stop("An unknown error has occurred, could not determine sunrise hour angle for an incline surface.")
  } 
  
  return(omega_rad)
}

# Angles in rad.
# TODO: Make this a hidden function.
sunrise_for_horizontal_surface = function(phi, delta, unit=1){
  
  # Equation 8 (1993): Sunrise hour angle [rad].
  omega_rad = -acos(-tan(phi) * tan(delta))
  return(omega_rad)
}

# The function.
#   Ls    - Areocentric longitude.
#   phi   - Planetary latitude.
#   unit  - Unit to return:
#           - 1 for radians.
#           - 2 for degrees.
#           - 3 for solar hour.
sunrise = function(Ls, phi, unit=1, beta=NULL, gamma_c=NULL){
  
  if(!unit %in% c(1, 2, 3)){
    stop("Sunrise unit option must either be 1 for radians, 2 for degrees, or 3 for solar hour.")
  }
  
  # The sunrise hour angle [rad] that will be calculated.
  omega_rad = NULL
  
  # Equation 7 (1990): Declination angle [rad].
  delta_rad = declination(Ls)
  
  # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
  # For polar nights and polar days:
  #   Polar night (polar_flag < -1), no solar irradiance.
  #   Polar day (polar_flag > 1), constant solar irradiance. 
  polar_flag = -tan(delta_rad) * tan(phi*pi/180)
  
  # If polar night or polar day, then there is no sunrise or sunset.
  if(polar_flag < -1 || polar_flag > 1){
    warning("Trying to get a sunrise hour angle during a polar night. This returns an NA which must be handled properly.")
    return(NA);
  }
  
  if((is.null(beta) && is.null(gamma_c))){
    omega_rad = sunrise_for_horizontal_surface(phi=phi*pi/180, delta=delta_rad, unit=unit)
    
  }else if(!is.null(beta) && !is.null(gamma_c)){
    if(gamma_c > 180 || gamma_c < -180){
      stop("Surface azimuth angle gamma_c must between -180 and 180 degress with zero south, east negative, and west positive.")
    }
    
    omega_rad = sunrise_for_inclined_surface(phi=phi*pi/180, beta=beta*pi/180, gamma_c=gamma_c*pi/180, delta=delta_rad)

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
# 
# d = declination(Ls)
# 
# for(p in seq(-30, 30, 10)){
#   for(b in seq(5, 35, 5)){
#   #  for(g in seq(-180, 180, 5)){
#   # for(b in c(35)){
#     for(g in c(1)){
#       # omega_sr = sunrise(Ls=Ls, phi=p, unit=1, beta=NULL, gamma_c=NULL)
#       # omega_sr_i = sunrise(Ls=Ls, phi=p, unit=1, beta=b, gamma_c=g)
#       # 
#       # T_sr = (omega_sr*180/pi + 180) / 15
#       # T_sr_i = (omega_sr_i*180/pi + 180) / 15
#       # 
#       # if(T_sr != T_sr_i){
#       #   cat("\n\n=====================================")
#       #   cat(paste("\n\nbeta:", b, ", gamma_c:", g))
#       #   cat(paste("\nT_sr:", T_sr, ", T_sr_i:", T_sr_i))
#       # }
#       
#       sr_east = sunrise_for_inclined_surface_oriented_east(phi=p*pi/180, beta=b*pi/180, gamma_c=g*pi/180, delta=d)
#       sr_west = sunrise_for_inclined_surface_oriented_west(phi=p*pi/180, beta=b*pi/180, gamma_c=g*pi/180, delta=d)
# 
#       if(sr_east != sr_west){
#         print(paste("sr_east", sr_east, "        sr_west:", sr_west))
#       }
#     }
#   }
# }

# beta: 35 , gamma_c: 30
# T_sr: -1.586734831866 , T_sr_i: -0.996317209799962


#######################################
# Testing code. To eventually remove. #
#######################################

# Ls = 300
# p = 20
# b = 45
# d = declination(Ls)
# 
# orientation_angles = seq(-180, 180, 1)
# sunrise_hours = c()
# 
# for(g in orientation_angles){
#   sr = sunrise(Ls, p, unit=3, beta=b, gamma_c=g)
#   sunrise_hours = c(sunrise_hours, sr)
# }
# 
# 
# dev.new()
# plot(orientation_angles, sunrise_hours,
#      ylab="<-- EARLIER      |      LATER -->",
#      xlab="<-- EAST      |      WEST -->")


