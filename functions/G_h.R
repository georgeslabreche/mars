# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Equation 4: Beam irridiance at the top of the Martian atmosphere (W/m2). 
Gob_eq = dget("functions/G_ob.R")

# The normalized net flux function.
f = dget("functions/f_89.R")

function(Ls, Z, tau, albedo=0.1){
  Gob_eq(Ls) * cos(Z * pi/180) * (f(Z,tau) / (1-albedo))
}
