# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#


library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Constrain T_start and T_end based on sunrise and sunset times.
constrain_solar_time_range = dget(here("utils", "constrain_solar_time_range.R"))

# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
# 
#   al  - NOT NEEDED - Included for looping convenience with other functions.
function(Ls, phi, tau, T_start, T_end, al=NULL, nfft){

  # Step 1: Constrain T_start and T_end based on sunrise and sunset times.
  
  # Apply solar time range constraint.
  T_range = constrain_solar_time_range(Ls, phi, T_start, T_end)

  # No solar irradiance within the contrained time range.
  if(is.null(T_range)){
    return(0)

  }else{
    # Constrain the time range.
    T_start = T_range$T_start
    T_end = T_range$T_end
  }
  
  # Step 2: Calculate beam insolation.

  # The interand for Equation 19 (1990).
  interand = function(T_s){

    Z = Z_eq(Ls=Ls, T_s=T_s, phi=phi, nfft=nfft)

    a = Gob_eq(Ls)
    b = cos(Z*pi/180)
    c = exp(-tau / cos(Z*pi/180))

    a * b * c
  }

  # Equation 19 (1990): Beam hourly insolation on Mars horizontal surface.
  I_bh = integrate(interand, T_start, T_end)

  return(I_bh$value)
}

