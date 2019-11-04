library(here)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

energy_divergences = get_energy_divergences(
  #Loss_shadowing = 0.07,
  #DustFactor_adjustment = 0.0825
)
# Selected Sols and the traverse direction:
#   Sol 4493: Due East.
#   Sol 4568: Due North-East.
#   Sol 4582: Due South-East.
#   Sol 4623: Due West.
#   Sol 4630: Due South-East.
energy_divergences_endeavour_rim = energy_divergences[energy_divergences$Sol %in% c(4493, 4568, 4582, 4623, 4630),]
