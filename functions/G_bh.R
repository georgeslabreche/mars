# Equation 18: Beam irradiance on Mars horizontal surface (W/m2).
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

library(here)

# Equation 4: Beam irridiance at the top of the Martian atmosphere (W/m2).
Gob_eq = dget(here("functions", "G_ob.R"))

function(Ls, Z, tau, al=NULL, nfft=NULL){
  Gob_eq(Ls) * cos(Z * pi/180) * exp(-tau / cos(Z * pi/180))
} 