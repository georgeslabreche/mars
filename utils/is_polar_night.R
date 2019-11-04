# Mars obliquity of rotation axis [deg].
delta_0 = 24.936

function(Ls, phi){
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
  
  # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
  #   Polar night (polar_flag < -1), no solar irradiance.
  #   Polar day (polar_flag > 1), constant solar irradiance. 
  polar_flag = -tan(delta) * tan(phi*pi/180)
  
  # If polar night.
  if(polar_flag < -1){
    # No solar irradiance.
    return(TRUE)
    
  }else{
    return(FALSE)
  }
}

