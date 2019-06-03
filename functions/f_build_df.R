# Build a dataframe representation of Table III referenced in From Appelbaum, Joseph & Flood, Dennis. (1990).
#
# From Appelbaum, Joseph & Flood, Dennis. (1990):
#   The net solar flux integrated over the solar spectrum on the Martian
#   surface was calculated by Pollack basd on multiple wavelength and
#   multiple scattering of the solar radiation. Derived data from this
#   calculation are shown Table III by the normalized net flux function
#   f(Z, tau) where the parameters are the zenith angle Z and the optical
#   depth tau. This table pertains to an albedo of 0.1 but can be used
#   for higher albedo values to a first approximation.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

function(){
  nnff = read.csv("data/normalized-net-flux-function-at-martian-surface.csv")
  rownames(nnff) = sprintf("%1.2f", nnff[,1])
  nnff = nnff[-c(1)]
}


