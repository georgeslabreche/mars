# Equation 16: The solar irradiance components on a horizontal Martian surface 
#
# Gh = Gbh + Gdh
# Determine an expression for the diffuse irradiance based on Eq. 17 and Eq. 18.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

library(here)

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget(here("functions", "G_h.R"))

# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
Gbh_eq = dget(here("functions", "G_bh.R"))

function(Ls, Z, tau, al, nfft){
  Gh_eq(Ls, Z, tau, al, nfft) - Gbh_eq(Ls, Z, tau)
} 