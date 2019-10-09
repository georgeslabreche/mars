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

oppy_status = read.csv(file=here("test", "data/oppy_status.csv"), header=TRUE, sep=",")

# Solar panel area is 1.3 m2 but actual cell area is 1.21 m2.
# Source: http://www.unmannedspaceflight.com/index.php?showtopic=1502
A = 1.21

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

# Plot differences between the predicted and ground truth Energy.
# dev.new()
# index = 1
# indices = c()
# for(Ls in results_df$Ls){
#   
#   if(Ls <= 360){
#     indices = c(indices, index)
#     index = index + 1
#     
#   }else{
#     # Plot line for the year
#     
#     # Reset index
#     index = 1
#   }
# }


tau_lines = function(Ls_vect, Sols, taus, sol_start, sol_end, include_years=NULL, cols=NULL){
  Ls_yr = c()
  Sols_yr = c()
  taus_yr = c()
  
  Ls_prev = 999
  year_count = 0
  index = 1
  
  for(sol in Sols){
    if(sol >= sol_start && sol <= sol_end){
      # Handle negative values of Ls.
      Ls = ifelse(c(Ls_vect[index])<0, c(Ls_vect[index])+360, c(Ls_vect[index]))
      
      # New year, Gone from Ls=360-ish to Ls=0-ish.
      if(Ls < Ls_prev){
        
        if(!is.null(Ls_yr)){
          if(is.null(include_years) || year_count %in% include_years){
            lines(Ls_yr, taus_yr, type = "l",
                  col= if(is.null(cols)) "black" else cols[year_count+1])
          }
        }
        
        Ls_yr = c(Ls)
        Sols_yr = c(Sols[index])
        taus_yr = c(taus[index])
        
        year_count = year_count + 1
        
      }else{
        # Still in the same year.
        Ls_yr = c(Ls_yr, Ls)
        Sols_yr = c(Sols_yr, Sols[index])
        taus_yr = c(taus_yr, taus[index])
      }
      
      Ls_prev = Ls
      index = index + 1
    }
  }
}

dev.new()
plot(1,
     xlab="Ls [deg]",
     ylab="Wh_diff",
     xlim=c(0, 360),
     ylim=c(-200, 200),
     type="l",
     lty=1,
     col="red")

tau_lines(Ls_vect=results_df$Ls, Sols=results_df$Sol, taus=results_df$Wh_diff, sol_start=min(results_df$Sol), sol_end=max(results_df$Sol))


# Write CSV
write.csv(results_df, file=here("test", "data/oppy_predicted_energy.csv"))


