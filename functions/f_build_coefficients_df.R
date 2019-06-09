# Build a dataframe representation of Table IV - Normalized Net Flux Function Coefficients in Appelbaum, Joseph & Flood, Dennis (1990) Update 1990. 
#
# Based on the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars: Update 1990. NASA STI/Recon Technical Report N. 91. 15117-.
# https://www.researchgate.net/publication/259222079_Solar_radiation_on_Mars_Update_1990

library(here)

function(k=0){
  nnff = read.csv(paste(here("data/normalized_net_flux_function/"), "k", k ,"_coefficients_1990_update.csv", sep=""))
  nnff = nnff[-c(1)]
}


