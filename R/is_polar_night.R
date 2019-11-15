#' Title
#'
#' @param Ls 
#' @param phi 
#'
#' @return
#' @export
is_polar_night = function(Ls, phi){
  
  # Equation 7 (1990): Declination angle [rad].
  delta = declination(Ls)
  
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

