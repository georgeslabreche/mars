# Equation 4: Beam irridiance at the top of the Martian atmosphere (W/m2). 
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

Ls_P = 248      # PERIPHELION - Dust Storm Season.
e = 0.093377    # Mars orbit eccentricity.
Mb = 590        # Mean beam irradiance at the top of the Martian atmosphere

function(Ls){
  Mb * ( (1 + e*cos( (Ls-Ls_P)* pi/180 ))^2 / (1-e^2)^2 ) # Eq. 4.
}
