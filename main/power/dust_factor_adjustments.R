# Using MER Opportunity data, figure out which dust factor adjustments need to be
# applied for different target error margin bounds.
#
# Results:
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

# -20%/+18% for a dust factor adjustment of 5% and shadowing loss of 0%. 

library(whisker)

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

result_message_template = 'For a {{DustFactor_adjustment}}% Dust Factor adjustment there are {{out_of_bounds}} out of bound measurements:
> {{upper_bound}}%: {{out_of_upper_bound}}
< {{lower_bound}}%: {{out_of_lower_bound}}
'

target_error_margin_lowest = -0.15
target_error_margin_highest = 0.21
  
# Try with dust factor adjustment from 0% to 15%
for(dfa in seq(0, 0.15, 0.005)){
  
  energy_divergences = get_energy_divergences(
    Loss_shadowing=0.05,
    DustFactor_adjustment=dfa)
  
  # Remove the outliers.
  #energy_divergences = energy_divergences[energy_divergences$WhDiffPercentage < 20, ]
  
  # Let's drop MY33 since it diverges so much compared to other years.
  divs_minus_MY33 = energy_divergences[energy_divergences$MarsYear != 33,]
  
  divs_out_of_upper_bound = divs_minus_MY33[divs_minus_MY33$WhDiffPercentage > target_error_margin_highest*100, ]
  divs_out_of_lower_bound = divs_minus_MY33[divs_minus_MY33$WhDiffPercentage < target_error_margin_lowest*100, ]
  
  out_of_bounds = length(divs_out_of_upper_bound$Ls) + length(divs_out_of_lower_bound$Ls)
  
  results = list(
    DustFactor_adjustment=dfa * 100,
    loss_shadowing = ls,
    upper_bound = target_error_margin_highest*100,
    lower_bound = target_error_margin_lowest*100,
    out_of_bounds = out_of_bounds,
    out_of_upper_bound = length(divs_out_of_upper_bound$Ls),
    out_of_lower_bound =  length(divs_out_of_lower_bound$Ls)
  )

  cat(whisker.render(result_message_template, results))
}
