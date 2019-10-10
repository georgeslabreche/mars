# Test the calculated generated energy [Wh] against ground truth measurements made by Opportunity.
#
# These tests will help calibrate the energy calculation's performance ratios relating to atmospheric opacity 
# and solar panel shading.
#
# Ground truth of Opportunity energy [Wh] was scraped from the rover's status update page: 
#   https://mars.nasa.gov/mer/mission/rover-status/opportunity

# TODO:
#   Plot:
#     Fix Ls values greater than 360.
#     Color plot lines.
#     Different types depending on storm year or not.
#     Overlay tau factor measurements in second x axis.
#
#   Calculation:
#     Figure out solar efficiency degredation, maybe mars effective is enough. Re-read paper.
#     Comfirm solar cell coverage area.
#     Estimate shadowing.

library(testthat)
library(here)

# Equation 12 (1990): Beam insolation on a horizotal surface at the top of Mars atmosphere [Wh/m2].
Ih_eq = dget(here("functions", "I_h.R"))

# Function to get Mars' Areocentric Longitude given a terrestrial date.
get_Ls = dget(here('utils', 'get_Ls.R'))

# Function to plot a vector of data into different groups of lines.
grouped_lines = dget(here("functions/plots", "grouped_lines.R"))

oppy_status = read.csv(file=here("test", "data/oppy_status.csv"), header=TRUE, sep=",")

# Solar panel area has been cited as 1.3 m2 but we need the actual solar cell coverage area.
#
# There is a total of 499 cells:
#   165 on the left wing.
#   167 on the right wing.
#   102 on the rear wing.
#   65 on the body.
#
# The cell size is 3.95 cm x 6.89 cm with two cropped corners. This provides an active area of 26.6 cm2.
# The CIC size is 3.97 cm x 6.91 cm.The slight difference is the small amount of coverglass overhang.
#   Source: E-mail exchange with Richard C. Ewell (NASA/JPL)
#
# The solar cell coverage area is thus 499 * 26.6 cm2 = 13273.4 cm2 = 1.32734 m2
# It turns out that the cited solar panel area is in actuality the solar cell coverage area.
A = 1.3

# Solar panel efficiency of MER solar array.
# GaInP/GaAs/Ge triple-junction solar cells 
# Source: https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=4060035
# Take into account degredation.
e = 0.22

# Opportunity's latitude location on Mars [deg].
# FIXME: Need to change this dynamically
phi = -2.1


# Prep results vector.
E_pr = c()
Ls_vect = c()

# Fetch Energy data from Opportunity status report.
# Compare it with predicted values.
for(i in 1:length(oppy_status$Sol)){
  
  # Terrestial date when the status update was made.
  date_terrestial = oppy_status$Date[i]
  
  # Tau factor.
  tau = oppy_status$TauFactor[i]
  
  # Solar array dust factor.
  # Perfectly clean solar arrays would have a dust factor of 1.0, so the larger the dust factor, the cleaner the arrays.
  dust_factor = oppy_status$SADustFactor[i]
  
  # Generated energy for the day [Wh]
  Wh = oppy_status$Wh[i]

  # Get the Areocentric longitude [deg] based on the date.
  Ls = get_Ls(date_terrestial, "%d-%b-%Y")
  Ls_vect = c(Ls_vect, Ls)
  
  if(!is.na(Ls) && !is.na(tau) && !is.na(dust_factor)){
    # Global hourly insolation on Mars horizontal surface [Wh/m2].
    Ih = Ih_eq(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, al=0.1, nfft=3)
    
    # Solar array performance ratio.
    PR = 1 - (0.03 + 0.05 + (1-dust_factor) + 0.05)
  
    # Calculate the generated energy [Wh] given the solar panel area [m2] and global insolation [Wh/m2].
    E = A * e * Ih * PR
    
    # Collected predicted energy into an array:
    E_pr = c(E_pr, E)
    
    #print(paste("E_pr = ", round(E), ", E_gt = ", Wh, ", diff_wh = ", round(E - Wh), sep=""))
    
  }else{
    E_pr = c(E_pr, NA)
  }
}

# Build new data table to present results.
# Remove rows that contain NA values.
results_df = na.omit(data.frame(
  Sol = oppy_status$Sol,
  Ls = Ls_vect,
  Date = oppy_status$Date,
  TauFactor = oppy_status$TauFactor,
  SADustFactor = oppy_status$SADustFactor,
  Wh_pr = E_pr,
  Wh_gt = oppy_status$Wh,
  Wh_diff = E_pr - oppy_status$Wh
))

# Prepare empty plot.
dev.new()
plot(1,
     xlab="Ls [deg]",
     ylab="Wh_diff",
     xlim=c(0, 360),
     ylim=c(-200, 200),
     type="l",
     lty=1,
     col="red")

# Plot Wh differences between predicted and measured.
grouped_lines(x=results_df$Ls, y=results_df$Wh_diff, x_floor=0, x_ceil=360, i=results_df$Sol)


# Write CSV
write.csv(results_df, file=here("test", "data/oppy_predicted_energy.csv"))


