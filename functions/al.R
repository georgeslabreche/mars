# Table I (Update 1991): The albedo function.
#
# Based on following publication:
#   Appelbaum, Joseph & Landis, Geoffrey & Sherman, I. (1991). Solar radiation on Mars—Update 1991. Solar Energy. 50. 35-51. 10.1016/0038-092X(93)90006-A:
#   https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991

# FIXME: Equation 3 (Update 1991)

al = read.csv(here("data/normalized_net_flux_function/", "albedo_1991_update.csv"))
rownames(al) = al[,1]
al = al[-c(1)]

# The albedo function.
#   longitude       - From -180 to 180 [deg].
#   latitude (phi)  - From -90 to 90 [deg].
function(longitude, latitude){
  if(longitude < -180 || longitude > 180){
    stop("Longitude must be a value between -180° and 180°.")
    
  }else if(latitude < -90 || latitude > 90){
    stop("Latitude (phi) must be a value between -90° and 90°.")
    
  }else if(longitude %% 10 != 0 || latitude %% 10 != 0 || latitude %% 10 != 0 || latitude %% 10 != 0){
    stop("Only vaues that are multiples of 10 are supported for longitude and latitude (phi).")
  }
  
  return(al[toString(longitude), paste("X", gsub("-", ".", latitude), sep="")])
}
