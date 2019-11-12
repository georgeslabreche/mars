# Equation 3 (1994): Global irradiance on Mars inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994).
#   Solar radiation on Mars: Tracking photovoltaic array.
#   Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://ntrs.nasa.gov/?R=19950004977

# FIXME: Update this function so that it figures out if its a polar night or day.

library(here)

# Equation 6 (1990): Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 14 (1990): Direct beam irradiance on the Mars surface normal to the solar rays [W/m2].
Gb_eq = dget(here("functions", "G_b.R"))

# Equation 16 (1990): Diffuse irrafiance on a horizontal Martian surface [W/m2].
Gdh_eq = dget(here("functions", "G_dh.R"))

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 7 (1990): The declination angle.
source(here("utils", "declination.R"))

# Equation 3 (1994): Global irradiance on an inclined surface.
#
#   Ls        - Areocentric longitude [deg].
#   omega     - Hour angle value [h].
#                 - An integer belonging to [6, 18] if using net flux function f_89 or f_90 (nfft 1 or 2).
#                 - An interger belonging to [0, 24] if using net flux function f_analytical (nfft 3).
#   phi       - Latitude [deg].
#   tau       - Optical depth.
#   al        - Albedo
#   beta      - Slope/Tilt angle [deg].
#   gamma_c   - Sun surface azimuth angle (i.e. orientation angle) [deg].
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
function(Ls, phi, T_s, Z=Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft), tau, al, beta, gamma_c, nfft){
  
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
  omega_deg = 15 * T_s - 180
  
  # Equation 6 (1993): Solar Azimuth Angle [deg]
  solar_azimuth_angle = function(){
    x = sin(phi*pi/180) * cos(delta) * cos(omega_deg*pi/180)
    y = cos(phi*pi/180) * sin(delta)
    z = sin(Z*pi/180)
    
    gamma_s = acos((x - y) / z) * 180/pi # [deg]
    
    # Alternatively, Equation 7 (1993): Solar Azimuth Angle [deg]
    # x = sin(phi*pi/180) * cos(Z*pi/180) - sin(delta)
    # y = cos(phi*pi/180) * sin(Z*pi/180)
    # 
    # gamma_s = acos(x / y) * 180/pi # [deg]
    
    return(gamma_s)
  } 
  
  # Set to omega to exactly zero when omega near zero instead because sometimes it is essentially zero with values like -2.8421709430404e-14 deg.
  for(i in 1:length(omega_deg)){
    if(omega_deg[i] > -0.1 && omega_deg[i] < 0.1){
      omega_deg[i] = 0
    }
  }
  
  # From (32) in (1993): It is solar noon when omega is 0 deg. This translates to gamma_s = 0 deg.
  gamma_s = ifelse(omega_deg == 0, 0, solar_azimuth_angle())
  
  
  # Sun Angle of Incidence [rad].
  #   Equation 13 (1993): Inclined surface.
  # FIXME: Equation 23 (1993): Vertical surface.
  #        Equation 27 (1993): Facing the equator.
  #        Equation 29 (1993): Beta = 0.
  sun_angle_of_incidence = function(){
    # if(beta == 0){ # Equation 29 (1993): Beta = 0.
    #   return(NULL)
    #   
    # }else if(beta == 90){ #Equation 29 (1993): Beta = 0.
    #   return(NULL)
    #   
    # }else if(gamma_c == 0){ # Equation 27 (1993): Facing the equator.
    #   return(NULL)
    #   
    # }else{
    #   #  Inclined surface.
    #   i = cos(beta * pi/180) * cos(Z * pi/180)
    #   j = sin(beta * pi/180) * sin(Z * pi/180) * cos((gamma_s - gamma_c) * pi/180) # Does not matter when beta = 0 because it leads to j = 0.
    #   teta = acos(i + j) # [rad]
    # }
    
    # Inclined surface.
    i = cos(beta * pi/180) * cos(Z * pi/180)
    j = sin(beta * pi/180) * sin(Z * pi/180) * cos((gamma_s - gamma_c) * pi/180) # Does not matter when beta = 0 because it leads to j = 0.
    teta = acos(i + j) # [rad]
    
    return(teta)
  }
  
  
  # Sun Angle of Incidence [rad] on an inclined surface.
  teta = sun_angle_of_incidence()
  
  # Equation 3 (1993): The global irradiance Gh_beta on an inclined surface with an angle beta.
  a = Gb_eq(Ls=Ls, Z=Z, tau=tau) * cos(teta)
  b = Gdh_eq(Ls=Ls, Z=Z, tau=tau, al=al, nfft=nfft) * cos((beta*pi/180) / 2)^2
  c = al * Gh_eq(Ls=Ls, Z=Z, tau=tau, al=al, nfft=nfft) * sin((beta*pi/180) / 2)^2
  
  # FIXME: Use this to account for polar nights and polar days as well as sunrise and sunset times.
  # a = Gb_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, nfft=nfft) * cos(teta)  
  # b = Gdh_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, nfft=nfft) * cos((beta*pi/180) / 2)^2
  # c = al * Gh_eq(Ls=Ls, phi=phi, T_s=T_s, tau=tau, al=al, nfft=nfft) * sin((beta*pi/180) / 2)^2
  
  Gh_beta = a + b + c

  return(Gh_beta)
}

