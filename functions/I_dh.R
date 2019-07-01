# Diffuse hourly insolation on Mars horizontal surface [Wh/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://ntrs.nasa.gov/?R=19890018252
#
library(here)

# Global hourly insolation on Mars horizontal surface [Wh/m2].
Ih_eq = dget(here("functions", "I_h.R"))

# Equation 19 (1990): Beam hourly insolation on Mars horizontal surface [Wh/m2].
Ibh_eq = dget(here("functions", "I_bh.R"))

function(Ls, phi, tau, T_start, T_end, al, nfft){
  Ih_eq(Ls, phi, tau, T_start, T_end, al, nfft) - Ibh_eq(Ls, phi, tau, T_start, T_end, al, nfft)
}