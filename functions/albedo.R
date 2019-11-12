# Table I (Update 1991): The albedo function.
#
# Based on following publication:
#   Appelbaum, Joseph & Landis, Geoffrey & Sherman, I. (1991). Solar radiation on Mars—Update 1991. Solar Energy. 50. 35-51. 10.1016/0038-092X(93)90006-A:
#   https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991

# FIXME: Equation 3 (Update 1991)
library(here)

# Avoid initialized the albedo data frame more than once.
if(!exists("albedo_df")){
  albedo_df = read.csv(here("data/albedo/", "albedo_1991_update.csv"))
  
  # Rename column labels (beta/phi). Use 'm' for minus/negative and 'p' for plus/positive
  colnames(albedo_df) = gsub("X\\.", "m", colnames(albedo_df))
  colnames(albedo_df) = gsub("X", "p", colnames(albedo_df))
  colnames(albedo_df) = gsub("p0", "0", colnames(albedo_df))
}

# The albedo function.
#   latitude (phi)  - From -90 to 90 [deg].
#   longitude       - From -180 to 180 [deg].
#   tau             - Optical depth.
albedo = function(latitude, longitude, tau, force_multiple_ten=FALSE){
  if(longitude < -180 || longitude > 180){
    stop("Longitude must be a value between -180° and 180°.")
    
  }else if(latitude < -90 || latitude > 90){
    stop("Latitude (phi) must be a value between -90° and 90°.")
    
  }else if(isTRUE(force_multiple_ten) && (longitude %% 10 != 0 || latitude %% 10 != 0 || latitude %% 10 != 0 || latitude %% 10 != 0)){
    stop("Coordinate values must be multiples of 10.")
  }
  
  # Round to a multiple of 10 because the lookup table only has mutiple of tens for longitude and latitude.
  longitude = round(longitude, digits=-1)
  latitude = round(latitude, digits=-1) 
  
  # Turn latitude numeric value into column label string value.
  if(latitude == 0){
    latitude = toString(latitude)
    
  }else if(latitude > 0){
    latitude = paste("p", latitude, sep="")
    
  }else if(latitude < 0){
    latitude = paste("m", abs(latitude), sep="")
  }
  
  # Get the albedo.
  al = albedo_df[albedo_df$Longitude == longitude, ][latitude][1,1]
  
  # For periods with dust storms, introduce the following albedo function from (3) in (1991 Update):
  if(tau > 0.5){
    al = max(c(al, min(c(0.18*tau, 0.4))))
  }
  
  return(al)
}
