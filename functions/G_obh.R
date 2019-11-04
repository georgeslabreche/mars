# Equation 5 (1990): Beam irridiance on a horizontal surface at the top of Mars atmosphere [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
library(here)

Gob_eq = dget(here("functions", "G_ob.R"))
Z_eq = dget(here("functions", "Z.R"))

function(Ls, phi=NULL, T_s=NULL, Z=Z_eq(Ls, T_s, phi, nfft), nfft=1){
  Gob_eq(Ls) * cos(Z*pi/180)
}