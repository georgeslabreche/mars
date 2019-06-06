# The normalized net flux function.
# Uses the looktup table published in "Solar radiation on Mars: Update 1990."
#   - Table III (a) and (b) for an albedo of 0.1.
#   - Table IV  (a) and (b) for an albedo of 0.4 
#
# TODO: Merge f_89.R, f_90.R, and f.R into a single parameterized function.
#
# Based on the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
# https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990

# The net flux function.
#   Z     - Zenith angle [deg].
#   tau   - Optical depth tau factor.
#   al    - Albedo (0.1 or 0.4).
function(Z, tau, al){
  stop("Not yet implemented.")
}