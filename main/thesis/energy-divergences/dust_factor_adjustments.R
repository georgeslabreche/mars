# Using MER Opportunity data, figure out which dust factor adjustments need to be
# applied for different target error margin bounds.
#
# Results:
#
# With outliers:
#
# -15%/+21% for a dust factor adjustment of 9.5% and shadowing loss of 5%.
# -15%/+21% for a dust factor adjustment of 7.35% and shadowing loss of 6%.
#
# -10%/+25% for a dust factor adjustment of 12.5% and shadowing loss of 5%.
# -10%/+25% for a dust factor adjustment of 10.25% and shadowing loss of 6%.
# -10%/+25% for a dust factor adjustment of 8.25% and shadowing loss of 7%.
#
# -23%/+15% for a dust factor adjustment of 4.9% and shadowing loss of 5%.
# -22%/+16% for a dust factor adjustment of 5.5% and shadowing loss of 5%.
#
# -20%/+18% for a dust factor adjustment of 5% and shadowing loss of 0%. 
#
#
# Without outliers that are less than -20% divergence:
#
# -12%/+13% for a dust factor adjustment of 3% and shadowing loss of 5%.
# -11%/+14% for a dust factor adjustment of 4% and shadowing loss of 5%.
# -10%/+15% for a dust factor adjustment of 4.5% and shadowing loss of 5%.

library(whisker)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

result_message_template = 'For a {{DustFactor_adjustment}}% Dust Factor adjustment there are {{out_of_bounds}} out of bound measurements:
> {{upper_bound}}%: {{out_of_upper_bound}}
< {{lower_bound}}%: {{out_of_lower_bound}}
'

# Select measurements with less than -20% divergence as the outliers.
# 1. Sols 2185 (-16%),  2199 (-31%), 2204 (peak at -32%), 2211 (-27%), 2218 (-26%), and 2226 (-16%).
# 2. Sol 2519 (Ls 242, 23-FEB-2011) of Mars Year 29? Divergence of -25%.
# 3. Sol 3901 (Ls 271, 13-JAN-2015) of Mars Year 32? Divergence of -21%.
Sol_outliers = c(2199, 2204, 2211, 2218, 2519, 3901)

target_error_margin_lowest = -0.10
target_error_margin_highest = 0.12
  
# Try with dust factor adjustment from 0% to 15%
for(dfa in seq(0.04, 0.15, 0.005)){
  
  energy_divergences = get_energy_divergences(
    Loss_shadowing=0.036,
    DustFactor_adjustment=dfa)
  
  # Let's drop MY33 since it diverges so much compared to other years.
  energy_divergences = energy_divergences[energy_divergences$MarsYear != 33,]
  
  # Remove the outliers.
  energy_divergences = energy_divergences[!(energy_divergences$Sol %in% Sol_outliers), ]
  
  # Count how many measurements are outside of the target margin of error range.
  divs_out_of_upper_bound = energy_divergences[energy_divergences$WhDiffPercentage > target_error_margin_highest*100, ]
  divs_out_of_lower_bound = energy_divergences[energy_divergences$WhDiffPercentage < target_error_margin_lowest*100, ]
  
  out_of_bounds = length(divs_out_of_upper_bound$Ls) + length(divs_out_of_lower_bound$Ls)
  
  # Organiye results into an object
  results = list(
    DustFactor_adjustment=dfa * 100,
    loss_shadowing = ls,
    upper_bound = target_error_margin_highest*100,
    lower_bound = target_error_margin_lowest*100,
    out_of_bounds = out_of_bounds,
    out_of_upper_bound = length(divs_out_of_upper_bound$Ls),
    out_of_lower_bound =  length(divs_out_of_lower_bound$Ls)
  )
  
  # Print result.
  cat(whisker.render(result_message_template, results))
}
