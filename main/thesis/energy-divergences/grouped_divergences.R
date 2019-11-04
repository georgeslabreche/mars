library(here)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

##############################################
# Group measurements into divergence groups. #
##############################################

# Get divergences for all Martian years except MY33.
energy_divergences = get_energy_divergences()
energy_divergences = energy_divergences[energy_divergences$MarsYear != 33,]

# To verify that datasets are not overlapping.
measurement_count = length(energy_divergences$Sol)
total_grouped_count = 0

floor = -30
ceiling = 10
step = 5

count = length(energy_divergences[energy_divergences$WhDiffPercentage < floor,]$Sol)
print(paste("Less than ", floor, "%: ", count, " (", round((count/ measurement_count)*100, 2), "%)", sep=""))

total_grouped_count = count

for(lower in seq(floor, ceiling-step, step)){
  upper = lower + step
  count = length(energy_divergences[energy_divergences$WhDiffPercentage > lower & energy_divergences$WhDiffPercentage < upper,]$Sol)
  print(paste(lower, "% to ", upper, "%: ", count, " (", round((count/ measurement_count)*100, 2), "%)", sep=""))
  
  total_grouped_count = total_grouped_count + count
}

count = length(energy_divergences[energy_divergences$WhDiffPercentage > ceiling,]$Sol)
print(paste("Greater than ", ceiling, "%: ", count, " (", round((count/ measurement_count)*100, 2), "%)", sep=""))

total_grouped_count = total_grouped_count + count

if(total_grouped_count == measurement_count){
  print("No data overlaps.")
}else{
  print(paste("Warning, ", total_grouped_count - measurement_count, " measurements overlap.", sep=""))
}
