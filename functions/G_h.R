# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

library(here)

# Equation 4: Beam irridiance at the top of the Martian atmosphere (W/m2). 
Gob_eq = dget(here("functions", "G_ob.R"))

# The normalized net flux functions.
f_89 = dget(here("functions", "f_89.R")) # f(Z,tau)
#f_90 = dget(here("functions", "f_90.R")) # f(Z,tau, al)
f = dget(here("functions", "f.R")) # f(Z,tau, al)

function(Ls, Z, tau, al, nfft){
  if(nfft == 1){
    if(al != 0.1){
      stop("The f_89 net flux function only supports an albedo of 0.1.")
    }
    
    net_flux = f_89(Z,tau)
    
  }else if(nfft == 2){
    if(al != 0.1 && al != 0.4){
      stop("The f_90 net flux function only supports an albedo of 0.1 or 0.4.")
    }
    
    #TODO: Implement.
    stop("Not yet implemented.")
    # net_flux = f_90(Z,tau,al)
    
  }else if(nfft == 3){
    net_flux = f(Z,tau,al)
    
  }else{
    stop("Unsupported net flux function type. Should be 1 for f_89, 2 for f_90, or 3 for f.")
  }
  
  Gob_eq(Ls) * cos(Z * pi/180) * (net_flux / (1-al))
}
