# Equation 14 (1990): Beam irradiance on Mars surface.
#
# From Appelbaum, Joseph & Flood, Dennis. (1990):
#   The direct beam irradiance, Gb, on the Martian surface normal to the solar rays
#   is related by Beer's law to the optical depth, tau, of the intervening atmospheric
#   haze [...] where m(z) is the air mass determined by the zenit angle Z.
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 14 (1990): Beam irradiance on Mars surface [W/m2]
#   Ls    - Areocentric Longitude.
#   Z     - Sun Zenith Angle.
#   tau   - Optical Depth.
function(Ls, Z, tau){
  Gob_eq(Ls) * exp(-tau * 1 / cos(Z * pi/180))
}