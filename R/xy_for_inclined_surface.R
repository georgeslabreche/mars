# From (1993): (19) for inclined and (25) for vertical surface.
# Angles in rad.
# TODO: Make this a hidden function.


#' Title
#'
#' @param phi planetary latitude [rad]
#' @param beta surface inclination angle [rad]
#' @param gamma_c orientation angle [rad]
#'
#' @return
x_for_inclined_surface = function(phi, beta, gamma_c){
  
  # FIXME: For vertical surface, i.e. beta = 90. Need gamma_s?!?!
  #        See (25) in (1993).
  a = ifelse(round(beta, 2) == round(90*pi/180, 2), 0, cos(phi) / (sin(gamma_c) * tan(beta)))
  b = sin(phi) / tan(gamma_c)
  
  x = a + b
  
  return(x)
}


# From (1993): (20) for inclined and (26) for vertical surface.
# Angles in rad.
# TODO: Make this a hidden function.


#' Title
#'
#' @param phi planetary latitude [rad]
#' @param beta surface inclination angle [rad]
#' @param gamma_c orientation angle [rad]
#' @param delta declination angle [rad]
#'
#' @return
y_for_inclined_surface = function(phi, beta, gamma_c, delta){
  
  # a is zero for a vertical surface, i.e. beta = 90 deg. See (26) from (1993)
  a = ifelse(round(beta, 2) == round(90*pi/180, 2), 0, sin(phi) / (sin(gamma_c) * tan(beta)))
  b = cos(phi) / tan(gamma_c)
  
  y = tan(delta) * (a - b)
  
  return(y)
}

# Ls=1*pi/180
# phi=-2*pi/180
# #beta=44.9851445949885*pi/180
# beta=1*pi/180
# gamma_c=0*pi/180
# 
# 
# delta_rad = declination(Ls)
# 
# x = x_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c)
# #y = y_for_inclined_surface(phi=phi, beta=beta, gamma_c=gamma_c, delta=delta_rad)
# 
# print(x)
# print(y)
