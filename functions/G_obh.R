# Equation 5 (1990): Beam irridiance on a horizontal surface at the top of Mars atmosphere [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
# TODO:
#   - Ls, phi, and nfft arguments to calculate Z. No need for irradiance check at top of atmosphere.
library(here)

Gob_eq = dget(here("functions", "G_ob.r"))

function(Ls, Z){
  Gob_eq(Ls) * cos(Z*pi/180)
}
