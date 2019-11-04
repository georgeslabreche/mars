# Equation 16 (1990):  The diffuse irradiance on a horizontal Martian surface.
#
# Gh = Gbh + Gdh
# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252

# FIXME: Update this function so that it figures out if its a polar night or day.

library(here)

# Equation 17 (1990):  Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 18 (1990):  Beam irradiance on Mars horizontal surface [W/m2].
Gbh_eq = dget(here("functions", "G_bh.R"))

# Check if there is irradiance based on the givent moment.
is_irradiated = dget(here("utils", "is_irradiated.R"))

# Equation 16 (1990): The solar irradiance components on a horizontal Martian surface [W/m2].
#
#   Ls        - Areocentric longitude [deg].
#   Z         - Sun zenith angle [deg].
#   tau       - Optical depth.
#   al        - Albedo.
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
function(Ls, phi=NULL, T_s=NULL, Z=Z_eq(Ls, T_s, phi, nfft), tau, al, nfft){
  if(!is_irradiated(Ls=Ls, phi=phi, T_s=T_s, Z=Z, nfft=nfft)){
    return(0)
    
  }else{
    G_dh = Gh_eq(Ls=Ls, phi=phi, T_s=T_s, Z=Z, tau=tau, al=al, nfft=nfft) - Gbh_eq(Ls=Ls, phi=phi, T_s=T_s, Z=Z, tau=tau, nfft=nfft)
    return(G_dh)
  }
} 