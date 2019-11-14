# From (1993): (19) for inclined and (25) for vertical surface.
# Angles in rad.
# TODO: Make this a hidden function.
x_for_inclined_surface = function(phi, beta, gamma_c){
  
  # FIXME: For vertical surface, i.e. beta = 90. Need gamma_s?!?!
  #        See (25) in (1993).
  a = cos(phi) / (sin(gamma_c) * tan(beta)) 
  a = ifelse(beta == 90*pi/180, 0, sin(phi) / (sin(gamma_c) * tan(beta)))
  b = sin(phi) / tan(gamma_c)
  
  x = a + b
  
  return(x)
}


# From (1993): (20) for inclined and (26) for vertical surface.
# Angles in rad.
# TODO: Make this a hidden function.
y_for_inclined_surface = function(phi, beta, gamma_c, delta){
  
  # a is zero for a vertical surface, i.e. beta = 90 deg. See (26) from (1993)
  a = ifelse(beta == 90*pi/180, 0, sin(phi) / (sin(gamma_c) * tan(beta)))
  b = cos(phi) / tan(gamma_c)
  
  y = tan(delta) * (a - b)
  
  return(y)
}