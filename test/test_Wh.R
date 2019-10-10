# Test the calculated generated energy [Wh] against ground truth measurements made by Opportunity.
#
# These tests will:
#   - Help calibrate the energy calculation's performance ratios relating to atmospheric opacity 
#     and solar panel shading.
#   - Provide an error margin for the Energy prediction calculations.
#
# Ground truth of Opportunity energy [Wh] was scraped from the rover's status update page: 
#   https://mars.nasa.gov/mer/mission/rover-status/opportunity

library(testthat)
library(here)

# Function to plot a vector of data into different groups of lines.
grouped_lines = dget(here("functions/plots", "grouped_lines.R"))

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

# Get data frame.
energy_divergences = get_energy_divergences()

# Write resulting data in a CSV file.
write.csv(energy_divergences, file=here("data", "opportunity_status/oppy_energy_divergences.csv"))
