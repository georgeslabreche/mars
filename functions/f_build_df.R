# Build a dataframe representation of Table III referenced in Appelbaum, Joseph & Flood, Dennis. (1990).
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
# Based on the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# al        - The albedo. Options are:
#               - 0.1 for pub_year 1990.
#               - 0.1 or 0.2 for pub_year 1991.
# pub_year  - The research paper's publication year. Retrieve data from the given publication year.
#             There were 2 updates after the origina publication in 1989/1990. Once in 1990 and another one in 1991.
function(al=0.1, pub_year=1990){
  csv_filename = NULL
  
  if(pub_year == 1990){
    if(al != 0.1){
      stop("Only an albedo of 0.1 is supported in the original 1990 publication of the normalized net flux function's lookup table.")
    }else{
      csv_filename = "table_III_1990.csv"
    }
    
  }else if(pub_year == 1991){
    if(al == 0.1){
      csv_filename = "al_0-1_table_III_1991_update.csv"
      
    }else if(al == 0.4){
      csv_filename = "al_0-4_table_III_1991_update.csv"
      
    }else{
      stop("Only an albedo of 0.1 or 0.4 is supported in the publication's 1991 update of the normalized net flux function's lookup table.")
    }
    
  }else{
    stop("Usupported publication year, should either be 1990 for the original pulication or 1991 for the 1991 update.")
  }
  
  nnff = read.csv(here("data/normalized_net_flux_function/", csv_filename))
  rownames(nnff) = sprintf("%1.2f", nnff[,1])
  nnff = nnff[-c(1)]
}


