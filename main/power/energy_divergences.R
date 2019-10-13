# Plot the divergence between predicted and measured energy aswatt-hours and percentage increase/descrease.
#
# Ground truth of Opportunity energy [Wh] was scraped from the rover's status update page: 
#   https://mars.nasa.gov/mer/mission/rover-status/opportunity
#
# TODO:
#   - What happened in Sol 2204 (6-APR-2010)? Huge divergence in energy. 2199, 2204 (peak), 2211, 2218. 
#     Traverse map:
#       Sol 2199: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2199_1.jpg
#       Sol 2204: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2204_1.jpg
#       Sol 2206: https://mars.nasa.gov/mer/mission/tm-opportunity/images/MERB_Sol2206_1.jpg
#
#     Some sort of arc turn. Maybe this arc turn was well synched with the panels.
#
#     "The rover attempted a drive on Sol 2202 (April 4, 2010). That drive stoppedafter the initial arc 
#     turn due to elevated current draw in the motors on the right side of the rover. The rover is between 
#     two ripples with the space in between forming a bowl. The rover had to push harder on the right to
#     make the sharp turn."
#
#     "With everything looking okay, another drive on Sol 2204 (April 6, 2010), was commanded. It too
#     began with a short, sharp arc. This time the drive stopped after a short distance because of wheel
#     slip exceeding the limit of 40 percent. Again ground controllers assessed the conditions and found no
#     problems. With these sharp turns, the rover's wheels must impart more thrust. When the wheel thrust
#     exceeds the shear strength of the terrain, slip occurs."
#
#
#
# Divergence factors:
#   - Shadow
#   - Slopes? 
library(here)
library(RColorBrewer)

# Function to plot a vector of data into different groups of lines.
grouped_lines = dget(here("functions/plots", "grouped_lines.R"))

# Function to get a data frame with all the divergence data between predictions and opportunity measurements.
get_energy_divergences = dget(here("utils", "get_energy_divergences.R"))

# Get data frame.
energy_divergences = get_energy_divergences()


# Plot divergences between predicted and measured.
# Positives values denote predictions that are greater than what was measured.
# Negative values denote predictions that are lesser than what was measured.
plot_divergences = function(x, y, i, ylab, ylim, ilim=NULL,
                            title=NULL,
                            legend){
  
  # Prepare empty plot.
  plot(1,
       xlab="Areocentric Longitude, Ls [deg]",
       ylab=ylab,
       xlim=c(0, 360),
       ylim=ylim,
       type="l",
       main=title)
  
  # Color options: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn
  cols = rev(brewer.pal(n=7, name="RdYlGn"))
  
  # Draw lines.
  grouped_lines(x=x, y=y, x_floor=0, x_ceil=360, i=i, ilim=ilim,
                cols=cols, lwd=4)
  
  # Draw legend
  legend("topleft", title="Mars Years",
         legend=legend,
         col=cols, lty=1, lwd=4)
}

# From MY28 to MY34 (i.e. data from Sol 1939 to 5100).
legend_MY28_to_MY34 = paste("MY", unique(energy_divergences$MarsYear), sep="")

# From MY28 to MY33 (i.e. data from Sol 1939 to 4718).
legend_MY28_to_MY33=paste("MY", unique(energy_divergences$MarsYear)[1:6], sep="")

# From MY28 to MY32 (i.e. data from Sol 1939 to 4037).
legend_MY28_to_MY32=paste("MY", unique(energy_divergences$MarsYear)[1:5], sep="")

###############################################
# Plot measured Tau Factor from MY29 to MY34. #
###############################################
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$TauFactor,
  i=energy_divergences$Sol,
  ylim=c(0, 2),
  ylab="Tau Factor",
  title="Opportunity's Measured Tau Factor",
  legend=legend_MY29_to_MY34)


############################################################
# Plot measured Solar Array Dust Factor from MY29 to MY34. #
############################################################
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$SADustFactor,
  i=energy_divergences$Sol,
  ylim=c(0.4, 1),
  ylab="Solar Array Dust Factor",
  title="Opportunity's Measured Solar Array Dust Factor",
  legend=legend_MY28_to_MY34)

##############################################################
# Plot predicted and measured energy, one plot per Mars Year #
##############################################################
dev.new()
par(mfrow = c(3, 2))  # 3 rows and 2 columns

# Create sequence of Mars Years we are interested to plot.
mars_years = unique(energy_divergences$MarsYear)

# Remove data from years 28 and 31, not enough data to be interesting.
mars_years = mars_years[!mars_years %in% c(28, 31)]

for (mars_year in mars_years) {
  year_energy_divergences = energy_divergences[energy_divergences$MarsYear == mars_year,]

  # Measured energy [Wh].
  plot(x=year_energy_divergences$Ls,
       y=year_energy_divergences$WhMeasured,
       xlab="Ls [deg]",
       ylab="Energy [Wh]",
       xlim=c(0, 360),
       ylim=c(200, 800),
       type="l",
       col=brewer.pal(n=3, name="RdYlGn")[1],
       main=paste("MY", mars_year, sep=""))

  # Predicted energy [Wh].
  lines(x=year_energy_divergences$Ls,
        y=year_energy_divergences$WhPredicted,
        lty=2,
        col=brewer.pal(n=3, name="RdYlGn")[3])

  # TODO: Draw legend.
}


#########################################################
# Plot predicted and measured energy from MY29 to MY34. #
#########################################################

# Plot predicted energy [Wh].
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhPredicted,
  i=energy_divergences$Sol,
  ylim=c(200, 800),
  ylab="Measured Energy [Wh]",
  title="Opportunity's Predicted Energy",
  legend=legend_MY28_to_MY34)

# Plot measured energy [Wh].
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhMeasured,
  i=energy_divergences$Sol,
  ylim=c(200, 800),
  ylab="Measured Energy [Wh]",
  title="Opportunity's Measured Energy",
  legend=legend_MY28_to_MY34)

#############################################
# Plot energy divergence from MY29 to MY34. #
#############################################
title="Opportunity's Predicted Energy Divergences from Daily Measurements"

# Plot divergence in Wh.
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhDiff,
  i=energy_divergences$Sol,
  ylim=c(-100, 220),
  ylab="Energy Divergence [Wh]",
  title=title,
  legend=legend_MY28_to_MY34)

# Plot divergence in percentage increase/decrease.
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhDiffPercentage,
  i=energy_divergences$Sol,
  ylim=c(-20, 50),
  ylab="Energy Divergence [%]",
  title=title,
  legend=legend_MY28_to_MY34)


############################################
# Plot energy divergence from MY29 to MY33 #
############################################

# Plot divergence in Wh.
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhDiff,
  i=energy_divergences$Sol,
  ylim=c(-100, 220),
  ilim=c(1939, 4056),
  ylab="Energy Divergence [Wh]",
  title=title,
  legend=legend_MY28_to_MY33)

# Plot divergence in percentage increase/decrease.
dev.new()
plot_divergences(
  x=energy_divergences$Ls,
  y=energy_divergences$WhDiffPercentage,
  i=energy_divergences$Sol,
  ylim=c(-20, 50),
  ilim=c(1939, 4056),
  ylab="Energy Divergence [%]",
  title=title,
  legend=legend_MY28_to_MY33)


