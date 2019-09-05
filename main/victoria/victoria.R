# Look at a past Mars mission with Opportunity's attempted descent into Victoria crater. 
# Entering the crater through an area called "Duck Bay", the rover was to zigzag back and forth across the crater's steep slope.
# However, on Sol 1600, one of the wheel motors drew an unexpected high level of current due to a short or an open circuit. 
# No natural obstruction was observed and the cause of the anomaly was never determined.
# The decision was made to exit the crater out of concern that the rover could not rely on only five wheels to get out. 
#
# Victoria crater's bay surfaces slope into the crater at an average of 19 deg.
# Using SherpaTT Utah field test data for a slope range from 9 to 28 deg, 
# an initial solar panel and battery sizing as well as a power budget analysis will be 
# presented on what it would take for SherpaTT to completely descend and then exit Victoria crater. 
#
# This baseline analysis will serve to plan power budgets for other mission scenarios to explore 
# sites of interest on the Martian surface.
#

# Load libraries.
library(here)
library(wesanderson)


# Global insolation
Hh_eq = dget(here("functions", "H_h.R"))

# Daylight range
sunrise = dget(here("utils", "sunrise.R"))
sunset = dget(here("utils", "sunset.R"))

# Planetary latitude of Victoria crater.
phi = -2.05 # Could estimate with 0

# Albedo.
al = 0.1    

# Net flux function type
#   1 for 1989 lookup table.
#   2 for 1990 lookup table.
#   3 for the analytical expresion.
nfft = 3

Ls_seq = 0:360
data_matrix = matrix(NA, nrow=length(Ls_seq), ncol=5)
colnames(data_matrix) = c('Ls', 'Sunrise', 'Sunset', 'Daylight', 'Insolation')

Ls_index = 1
for(Ls in Ls_seq){
  H_h = Hh_eq(Ls=Ls, phi=phi, tau=tau, nfft=nfft)
  T_sr = sunrise(Ls, phi, 3)
  T_ss = sunset(Ls, phi, 3)
  
  data_matrix[Ls_index, 1] = Ls           # deg
  data_matrix[Ls_index, 2] = T_sr         # hours
  data_matrix[Ls_index, 3] = T_ss         # hours
  data_matrix[Ls_index, 4] = T_ss - T_sr  # hours
  data_matrix[Ls_index, 5] = H_h          # Wh/m2-deg
  
  Ls_index = Ls_index + 1
}

dev.new()
plot(x=data_matrix[, 'Ls'], y=data_matrix[, 'Daylight'],
     type='l',
     xlab='Areocentric Longitude, Ls [deg]',
     ylab='Daylight [h]',
     main='Daylight hours as a function of Areocentric Longitude')

dev.new()
plot(x=data_matrix[, 'Ls'], y=data_matrix[, 'Insolation'],
     type='l',
     xlab='Areocentric Longitude, Ls [deg]',
     ylab='Insolation [Wh/m2-deg]',
     main='Global Insolation as a function of Areocentric Longitude')


  
