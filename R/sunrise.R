# Get the moment in which the sunrise occurs.
# FIXME: 
#   1. For surface oriented towards the equator, gamma_c = 0 or 180/-180
#   2. For vertical surface (when Beta=90)

# (30) in (1993).
# Angles in rad.
# TODO: Make this a hidden function.
# FIXME: Not exactly what is in (21) to determine omega_rad. 
#' Title
#'
#' @param phi 
#' @param beta 
#' @param delta 
#'
#' @return
#' @export
sunrise_for_inclined_surface_oriented_equator = function(phi, beta, delta){
  
  # (8) in (1993).
  omega_rad_1 = sunrise_for_horizontal_surface(phi=phi, delta=delta)

  # (30) in (1993).
  omega_rad_2 = -acos(-tan(delta) * tan(phi-beta))

  # From (21) in (1993) we want to grab the minium between (8) and (30).
  omega_rad = -min(abs(omega_rad_1), abs(omega_rad_2))
  
  # FIXME: Is it just (30)? This was checked with plots.
  #omega_rad = -acos(-tan(delta) * tan(phi-beta))
  
  return(omega_rad)
}

# (15) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
#' Title
#'
#' @param phi 
#' @param beta 
#' @param gamma_c 
#' @param delta 
#'
#' @return
#' @export
sunrise_for_inclined_surface_oriented_east = function(phi, beta, gamma_c, delta){
  
  # Calculate omega for horizontal surface.
  omega_rad_1 = sunrise_for_horizontal_surface(phi=phi, delta=delta)

  # Calculate omega for inclined surface.
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)

  # If the radicand is negative then it means that the sun never rises on the inclined surface.
  # This can be the case of the inclined surface is at a:
  #   - high planetary latitude and oriented northwards.
  #   - low planetary latitude and oriented southwards.
  radicand = x^2 - y^2 + 1
  if(radicand < 0){
    return(NA)
  }
  
  a = -x*y - sqrt(radicand)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  # Pick between the two calculated omegas.
  omega_rad = -min(abs(omega_rad_1), omega_rad_2)
  
  return(omega_rad)
}


# (17) from (1993).
# Angles in rad.
# TODO: Make this a hidden function.
#' Title
#'
#' @param phi 
#' @param beta 
#' @param gamma_c 
#' @param delta 
#'
#' @return
#' @export
sunrise_for_inclined_surface_oriented_west = function(phi, beta, gamma_c, delta){
  
  # Calculate omega for horizontal surface.
  omega_rad_1 = sunrise_for_horizontal_surface(phi=phi, delta=delta)
  
  # Calculate omega for inclined surface.
  x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
  y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
  
  # If the radicand is negative then it means that the sun never rises on the inclined surface.
  # This can be the case of the inclined surface is at a:
  #   - high planetary latitude and oriented northwards.
  #   - low planetary latitude and oriented southwards.
  radicand = x^2 - y^2 + 1
  if(radicand < 0){
    return(NA)
  }
  
  a = -x*y + sqrt(radicand)
  b = x^2 + 1
  
  omega_rad_2 = acos(a / b)
  
  # Pick between the two calculated omegas.
  omega_rad = -min(abs(omega_rad_1), omega_rad_2)
  
  return(omega_rad)
}

# Angles in rad.
# TODO: Make this a hidden function.
#' Title
#'
#' @param phi 
#' @param delta 
#'
#' @return
#' @export
sunrise_for_horizontal_surface = function(phi, delta){
  
  # Equation 8 (1993): Sunrise hour angle [rad].
  omega_rad = -acos(-tan(delta) * tan(phi))
  
  return(omega_rad)
}

# Angles in rad.
# TODO: Make this a hidden function.
#' Title
#'
#' @param phi 
#' @param beta 
#' @param gamma_c 
#' @param delta 
#'
#' @return
#' @export
sunrise_for_inclined_surface = function(phi, beta, gamma_c, delta){
  
  # Inclination angle is 0 degrees, this is equivalent to a horizontal surface.
  if(beta == 0){
    omega_rad = sunrise_for_horizontal_surface(phi=phi, delta=delta)
    
  }else if(gamma_c == 0){
    # Inclined surface facing the equator.
    # i.e. Oriented South when in the Northern hemisphere and oriented North when in the Southern Hemisphere.
    omega_rad = sunrise_for_inclined_surface_oriented_equator(phi=phi, beta=beta, delta=delta)
    
  }else if(round(abs(gamma_c), 2) == round(pi, 2)){
    # Inclined surface facing opposite the equator.
    # i.e. Oriented North when in the Northern hemisphere and Oriented South whn in the Southern Hemisphere.
    omega_rad = sunrise_for_inclined_surface_oriented_equator(phi=phi, beta=beta, delta=delta)
    
  }else if(gamma_c < 0){ # Inclined surface is oriented towards the East.
    omega_rad = sunrise_for_inclined_surface_oriented_east(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else if(gamma_c > 0){ # Incline surface is oriented towards the West.
    omega_rad = sunrise_for_inclined_surface_oriented_west(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta)
    
  }else{
    # This should not happen.
    stop(paste("An unknown error has occurred. Could not determine sunrise hour angle for an inclined surface when phi=", phi, " beta=", beta, " and gamma_c=", gamma_c, ".", sep=""))
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
#' Title
#'
#' @param Ls 
#' @param phi 
#' @param beta 
#' @param gamma_c 
#' @param unit 
#'
#' @return
#' @export
sunrise = function(Ls, phi, beta=NULL, gamma_c=NULL, unit=1){
  
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
    omega_rad = sunrise_for_horizontal_surface(phi=phi*pi/180, delta=delta_rad)
    
  }else if(!is.null(beta) && !is.null(gamma_c)){
    if(gamma_c > 180 || gamma_c < -180){
      stop("Surface azimuth angle gamma_c must between -180 and 180 degrees with zero south, east negative, and west positive.")
    }
    
    omega_rad = sunrise_for_inclined_surface(phi=phi*pi/180, beta=beta*pi/180, gamma_c=gamma_c*pi/180, delta=delta_rad)

  }else{
    stop("Invalid argument values. Both beta and gamma_c should either be NULL or not NULL.")
  }
  
  # If the radicand is negative then it means that the sun never rises on the inclined surface.
  # This can be the case of the inclined surface is at a:
  #   - high planetary latitude and oriented northwards.
  #   - low planetary latitude and oriented southwards.
  if(is.na(omega_rad)){
    return(NA)
  }
  
  # Get the hour angle in degrees.
  omega_deg = omega_rad * 180/pi
  
  # Equation 8 (1990): Hour angle. Determine the Surise and Sunset times [h].
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  Ts = (omega_deg + 180) / 15
  
  if(unit == 1){
    return(omega_rad)
    
  }else if(unit == 2){
    return(omega_deg)
    
  }else if(unit == 3){
    return(Ts)
    
  }else{
    # This should not happen.
    stop("An unknown error has occurred.")
  }  
}

# Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)
# 
# Ls_seq=1:360
# Ls=27
# phi=34
# 
# #beta=45
# gamma_c=140
# 
# Ls = 1
# Ls_seq = 1:360
# phi = -10
# beta = 1
# gamma_c = 0
# 
# index = 1
# for(gamma_c in gamma_c){
# #for(phi in seq(45, 50, 5)){
# 
#   sr_vect = c()
#   ss_vect = c()
#   for(Ls in Ls_seq){
# 
#     delta_rad = declination(Ls)
#     beta = optimal_angle(Ls=Ls, phi=phi, unit=2)
# 
#     #cs = constrain_solar_time_range(Ls=Ls, phi=phi, Ts_start=0, Ts_end=24, beta=beta, gamma_c=gamma_c)
# 
#     #print(paste("Ls ", Ls, ", phi ", phi, ", beta ", beta))
#     #print(cs)
# 
#     sr = sunrise(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
#     ss = sunset(Ls=Ls, phi=phi, beta=beta, gamma_c=gamma_c, unit=3)
#     sr_vect = c(sr_vect, sr)
#     ss_vect = c(ss_vect, ss)
#     #print(paste(gamma_c, " ", sr))
# 
#   }
# 
#   if(index == 1){
#     dev.new()
#     plot(Ls_seq, sr_vect, ylim=c(0,20),
#          type="l", lwd=2, xlab="Areocentric Longitude [deg]", ylab="Surface sunrise time [hr]")
# 
#     lines(Ls_seq, ss_vect, lwd=2)
#   }else{
#     lines(Ls_seq, sr_vect, lwd=2)
#     lines(Ls_seq, ss_vect, lwd=2)
#   }
# 
#   index = index + 1
# }