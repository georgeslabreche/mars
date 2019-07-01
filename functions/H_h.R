# Global daily insolation on Mars horizontal surface [Wh/m2-day].
#
# Obtained by integrating I_h, global hourly insolation on Mars horizontal surface, 
# over the period from sunrise to sunset.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#

library(here)

# Global hourly insolation on Mars horizontal surface [W/m2-h].
Ih_eq = dget(here("functions", "I_h.R"))

function(Ls, phi, tau, al=0.1, nfft){
  # H_h is obtained by integrating I_h over the period from sunrise to sunset.
  Ih_eq(Ls, phi, tau, 0, 24, al, nfft)
}