# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

library(here)

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# The normalized net flux function
f = dget(here("functions", "f.R"))

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
#   Ls        - Areocentric longitude [deg].
#   Z         - Sun zenith angle [deg].
#   tau       - Optical depth.
#   al        - Albedo.
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
function(Ls, Z, tau, al, nfft){
  if(nfft == 1){
    net_flux = f(Z, tau, al, pub_year=1989)
    
  }else if(nfft == 2){
    net_flux = f(Z, tau, al, pub_year=1990)
    
  }else if(nfft == 3){
    net_flux = f(Z, tau, al)
    
  }else{
    stop("Unsupported net flux function type. Should be 1 for the original 1989 lookup table publication, 2 for the 1990/1991 lookup table update, or 3 for the analytical expression.")
  }
  
  Gob_eq(Ls) * cos(Z * pi/180) * (net_flux / (1-al))
}
