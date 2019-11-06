# Build a data frame that presents predicted energy for Opportunity versus measured energy.
# The data frame also contains the divergence between predicted and measured energy as watt-hours and percentage increase/descrease.
#
# Ground truth of Opportunity energy [Wh] was scraped from the rover's status update page: 
#   https://mars.nasa.gov/mer/mission/rover-status/opportunity
#

library(here)

# Equation 12 (1990): Beam insolation on a horizontal surface at the top of Mars atmosphere [Wh/m2].
Ih_eq = dget(here("functions", "I_h.R"))

# Function to get Mars' Areocentric Longitude given a terrestrial date.
get_Ls = dget(here('utils', 'get_Ls.R'))

# Opportunity status data will be read from a CSV file.
# Don't bother reading it again if this has been done already
if(exists('oppy_status') == FALSE){
  oppy_status = read.csv(file=here("data", "opportunity_status/oppy_status.csv"), header=TRUE, sep=",")
}

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
#   Source: E-mail exchange with Richard C. Ewell (NASA/JPL).
#
# The solar cell coverage area is thus 499 * 26.6 cm2 = 13273.4 cm2 = 1.32734 m2
# It turns out that the cited solar panel area is in actuality the solar cell coverage area.
A = 1.3

# MER Rover solar panel have  GaInP/GaAs/Ge triple-junction solar cells.
# Their efficiency is 22% on Mars surface.
#   Source: https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=4060035
e = 0.22

# Opportunity's latitude location on Mars [deg].
phi = -2.05

#
# Fetch Energy data from Opportunity status report.
# Compare it with predicted values and calculate divergences.
#
# Performance ratio / coefficient for losses is determined based on literature:
#
#   1. Thomas W. Kerslake et al.:
#       On a given day, cell efficiency varies 3% due to changing temperature and
#       red - shift spectral losses through the day time-period.
#
#   2. Geoffrey A. Landis et al.: 
#       2.1 Dust deposition on the solar arrays was measured on the Pathfinder
#           mission to to degrade the performance at a rate of 0.28% per sol
#           during the initial 30 sols of the mission.
#       2.2 Longer measures on MER missions indicate that long-term degradation
#            is about 0.14% per sol.
#
#   3. Jeremiah McNatt et al.:
#       After deployment, a 5% permanent dust power loss is added to the assumption
#       with more accumulated dust removed periodically.
#
#   4. Paul M. Stella et al.
#       Dust performance degradation is about 30%.
#
# By default, use [1] and [3].
# Also use dust factor measurements taken by Opportunity instead of [2] and [4].
# Assume 5% loss due to shadowing onthe panels (e.g. from the mastcam).
# 
#   Loss_temp_redshit:      Cell efficiency varies due to changing temperature and red shift.
#   Loss_permanent_dust:    After deployment, a permanent dust power loss is added to the assumption
#                           with more accumulated dust removed periodically.
#   Loss_shadowing:         Loss due to shadows on the solar call cast devices such as the rover mast and mastcam.
#   SolarArray_DustFactor:  Perfectly clean solar arrays would have a dust factor of 1.0, so the larger the dust factor,
#                           the cleaner the arrays.
#   DustFactor_adjustment:  The Dust factor adjustment is a percentage value that can be used
#                           to bound the difference between min and max diverences.
#                           e.g. A dust factor adjustment of 8% will bound the divergence between
#                           predicted energy and measured energy to an error margin from -20% to +20%.
function(
  Loss_temp_redshit = 0.03,    # 3% from [1].
  Loss_permanent_dust = 0.05,  # 5% from [3].
  Loss_shadowing = 0.05,       # 5% assumption.
  SolarArray_DustFactor = NULL,
  DustFactor_adjustment = 0){
  
  # Prep results vector.
  E_pr = c()
  Ls_seq = c()
  MarsYears = c()
  
  for(i in 1:length(oppy_status$Sol)){
    
    # Get sol.
    sol = oppy_status$Sol[i]
    
    # Determine Martian year based on given Sol.
    if(sol >= 1939 && sol <= 2042){
      MarsYears = c(MarsYears, 28)
      
    }else if(sol >= 2049 && sol <= 2709){
      MarsYears = c(MarsYears, 29)
      
    }else if(sol >= 2716 && sol <= 3384){
      MarsYears = c(MarsYears, 30)
      
    }else if(sol >= 3390 && sol <= 3437){
      MarsYears = c(MarsYears, 31)
      
    }else if(sol >= 3444 && sol <= 4037){
      MarsYears = c(MarsYears, 32)
      
    }else if(sol >= 4055 && sol <= 4718){
      MarsYears = c(MarsYears, 33)
      
    }else{
      MarsYears = c(MarsYears, 34)
    }

    
    # Terrestial date when the status update was made.
    date_terrestial = oppy_status$TerrestialDate[i]
    
    # Tau factor.
    tau = oppy_status$TauFactor[i]
    
    # Solar array dust factor.
    # Perfectly clean solar arrays would have a dust factor of 1.0, so the larger the dust factor, the cleaner the arrays.
    if(is.null(SolarArray_DustFactor)){
      SA_DustFactor = oppy_status$SADustFactor[i]
    }else{
      SA_DustFactor = SolarArray_DustFactor
    }
    
    # Generated energy for the day [Wh]
    Wh = oppy_status$Wh[i]
    
    # Get the Areocentric longitude [deg] based on the date.
    Ls = get_Ls(date=date_terrestial,
                format="%d-%b-%Y",
                force_bounding=TRUE)
    
    Ls_seq = c(Ls_seq, Ls)
    
    if(!is.na(Ls) && !is.na(tau) && !is.na(SA_DustFactor)){
      # Global hourly insolation on Mars horizontal surface [Wh/m2].
      Ih = Ih_eq(Ls=Ls, phi=phi, tau=tau, T_start=0, T_end=24, al=0.1, nfft=3)
      
      # Performance ratio.
      PR = 1 - (Loss_temp_redshit + Loss_permanent_dust + (1-(SA_DustFactor*(1-DustFactor_adjustment))) + Loss_shadowing)
      
      # Calculate the generated energy [Wh] given the solar panel area [m2] and global insolation [Wh/m2].
      E = A * e * Ih * PR
      
      # Collected predicted energy into an array:
      E_pr = c(E_pr, E)
      
    }else{
      E_pr = c(E_pr, NA)
    }
  }
  
  # Build new data table to present results.
  # Remove rows that contain NA values.
  divergences = na.omit(data.frame(
    TerrestialDate = oppy_status$TerrestialDate,
    MarsYear = MarsYears,
    Ls = Ls_seq,
    Sol = oppy_status$Sol,
    TauFactor = oppy_status$TauFactor,
    SADustFactor = oppy_status$SADustFactor,
    WhPredicted = E_pr,
    WhMeasured = oppy_status$Wh,
    WhDiff = E_pr - oppy_status$Wh,
    WhDiffPercentage = ((oppy_status$Wh - E_pr) / oppy_status$Wh) * 100
  ))
  
  return(divergences)
}
