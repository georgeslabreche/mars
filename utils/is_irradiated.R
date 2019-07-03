# Check if there is solar irradiance at the given location and moment.

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 8 (1993): Sunrise.
sunrise = dget(here("utils", "sunrise.R"))

# Equation 9 (1990): Sunset.
sunset = dget(here("utils", "sunset.R"))

# Mars obliquity of rotation axis [deg].
delta_0 = 24.936

function(Ls, phi, T_s, Z=Z_eq(Ls, T_s, phi, nfft), nfft){
  
  if(isTRUE(identical(Z, numeric(0)))){
    stop("One of the following is required: i. Sun zenith angle Z [deg] or ii. Both latitude phi [deg] and solar time T_s [h].")
    
  }else if(!is.null(phi) && !is.null(T_s) && Z != Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)){
    warning("Sun zenith angle Z [deg] has been provided, ignoring given latitude phi [deg] and solar time T_s [h].")
    
  }else if(is.null(phi) && !is.null(T_s) || !is.null(phi) && is.null(T_s)) {
    warning("A latitude phi [deg] or a solar time T_s [h] has been given but not needed because a Sun zenith angle Z [deg] has been given as well.")
    
  }else if(!is.null(phi) && !is.null(T_s)){
    
    # Equation 7 (1990): Declination angle [rad].
    delta = asin(sin(delta_0*pi/180) * sin(Ls*pi/180))
    
    # Equations 16 (Update 1991): Figure out if it is polar night or polar day.
    #   Polar night (polar_flag < -1), no solar irradiance.
    #   Polar day (polar_flag > 1), constant solar irradiance. 
    polar_flag = -tan(delta) * tan(phi*pi/180)
    
    # There is no irradiance during polar nights.
    if(polar_flag < -1){
      return(FALSE)
      
    }else if(polar_flag > 1){
      # Constant solar irradiance during polar day.
      # The sun is out all the time.
      return(TRUE)
      
    }else{
      # There is no irradiance if the solar time is before sunrise or after sunset.
      sr = sunrise(Ls, phi, 3)
      ss = sunset(Ls, phi, 3)
      
      if(T_s < sr || T_s > ss){
        return(FALSE)
        
      }else{
        return(TRUE)
      }
    }
  }
  
  if(Z >= 90){
    return(FALSE)
    
  }else{
    return(TRUE)
  }
}