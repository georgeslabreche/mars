# Equation 14 (1990): Direct beam irradiance on the Mars surface normal to the solar rays.
#
# From Appelbaum, Joseph & Flood, Dennis. (1990):
#   The direct beam irradiance, Gb, on the Martian surface normal to the solar rays
#   is related by Beer's law to the optical depth, tau, of the intervening atmospheric
#   haze [...] where m(z) is the air mass determined by the zenith angle Z.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Check if there is irradiance based on the givent moment.
is_irradiated = dget(here("utils", "is_irradiated.R"))

# Equation 14 (1990): Beam irradiance on Mars surface [W/m2]
#   Ls    - Areocentric Longitude.
#   Z     - Sun Zenith Angle.
#   tau   - Optical Depth.
function(Ls, phi=NULL, T_s=NULL, Z=Z_eq(Ls, T_s, phi, nfft), tau, nfft){
  if(!is_irradiated(Ls=Ls, phi=phi, T_s=T_s, Z=Z, nfft=nfft)){
    return(0)
    
  }else{
    Gob_eq(Ls) * exp(-tau / cos(Z*pi/180))
  }
}