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
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# File with data from 1989 table which has 1990 in filename
# because that's the official publication year for the original document written in 1989.
if(!exists("nnff_01_1989")){
  nnff_01_1989 = read.csv(here("data/normalized_net_flux_function/", "table_III_1990.csv"))
}

# File with data from 1991 table which is the same as that of 1990.
if(!exists("nnff_01_1991")){
  nnff_01_1991 = read.csv(here("data/normalized_net_flux_function/", "al_0-1_table_III_1991_update.csv"))
}

# File with data from 1991 table which is the same as that of 1990.
if(!exists("nnff_04_1991")){
  nnff_04_1991 = read.csv(here("data/normalized_net_flux_function/", "al_0-4_table_III_1991_update.csv"))
}

# The function.
#
#   al        - The albedo. Options are:
#                 - 0.1 for pub_year 1989.
#                 - 0.1 or 0.2 for pub_year 1990.
#   pub_year  - The research paper's publication year. Retrieve data from the given publication year.
#               There were 2 updates after the original publication in 1989. Once in 1990 and another one in 1991.
#               We only consider the 1990 update since the 1991 update brought no changes to the net flux lookup table.
function(al=0.1, pub_year=1989){
  
  if(pub_year == 1989){
    if(al != 0.1){
      stop("Only an albedo of 0.1 is supported in the original 1989 publication of the normalized net flux function's lookup table.")
    
    }else{
      return(nnff_01_1989)
    }
  }
  else if(pub_year == 1990){
    if(al == 0.1){
      return(nnff_01_1991)
      
    }else if(al == 0.4){
      return(nnff_04_1991)
      
    }else{
      stop("Only an albedo of 0.1 or 0.4 is supported in the publication's 1990 update of the normalized net flux function's lookup table.")
    }
    
  }else{
    stop("Usupported publication year, should either be 1989 for the original pulication or 1990 for its 1990 update.")
  }
}


