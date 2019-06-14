# Build a dataframe representation of Table IV - Normalized Net Flux Function Coefficients in Appelbaum, Joseph & Flood, Dennis (1990) Update 1990. 
#
# Based on the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
# https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990

library(here)

# Load data from files.
nnff_k0 = read.csv(here("data/normalized_net_flux_function/", "k0_coefficients_1990_update.csv"))
nnff_k1 = read.csv(here("data/normalized_net_flux_function/", "k1_coefficients_1990_update.csv"))

function(k=0){
  if(k==0){
    nnff_k0 = nnff_k0[-c(1)]
    return(nnff_k0)
    
  }else if(k==1){
    nnff_k1 = nnff_k1[-c(1)]
    return(nnff_k1)
    
  }else{
    paste("Unsupported value for k, must be either 0 or 1.")
  }
}


