library(here)
library(whisker)

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Global daily insolation on Mars horizontal surface [Wh/m2-day].
Hh_eq = dget(here("functions", "H_h.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R")) 

sunrise = dget(here("utils", "sunrise.R"))
sunset = dget(here("utils", "sunset.R")) 

# InSight generated 4,588 watt-hours on its first sol.
# Phoenix lander generated around 1,800 watt-hours in a day.

# Define a class to encapsulate rover power consumption details.
RoverPower = setClass(
  "RoverPower",
  slots=list(name="character",
             propulsion="numeric",
             base="numeric",
             hibernate="numeric"))

# create a method to get the total power.
setGeneric(name="total",
           def=function(object)
           {
             standardGeneric("total")
           }
)

setMethod(f="total",
          signature="RoverPower",
          definition=function(object)
          {
            total = object@propulsion + object@base
            return(total)
          }
)

# Instanciate class for MER and Sherpa rovers.
mer = new("RoverPower", name="MER", propulsion=100, base=40, hibernate=40)
sherpa = new("RoverPower", name="SherpaTT", propulsion=216, base=160, hibernate=100)


Ls_list = list(
  "Vernal Equinox" = 0,
  "Aphelion" = 71,
  "Summer Solstice" = 90,
  "Dust Storm Season Begins" = 180,
  "Autumn Equinox" = 180,
  "Periphelion" = 248,
  "Winter Solstice" = 270,
  "Dust Storm Season Ends" = 330)

phi_list = list(
  'Nanedi Vallis' = 5.2,
  'Naktong Vallis' = 5.3,
  'Melas Coprates' = -10.4,
  'Kasei Valles' = 24.6
)

phi = phi_list$'Nanedi Vallis'

Ls_global_dust_storm_season = Ls_list$'Dust Storm Season Begins':Ls_list$'Dust Storm Season Ends'
Ls_non_global_dust_storm_season = c(1:Ls_list$'Dust Storm Season Begins', Ls_list$'Dust Storm Season Ends':360)


e = 29  # Solar panel efficiency [%].
G = 0   # Solar flux / Irradiance [W/m2].

# Performance ratio / coefficient for losses is determined based on literature:
#
#   1. Thomas W. Kerslake et al.:
#       On a given day, cell efficiency varies 3% due to changing temperature and
#       red-shift spectral losses through the day time-period.
#
#   2. Geoffrey A. Landis et al.: 
#       2.1 Dust deposition on the solar arrays was measured on  the Pathfinder
#           mission to to degrade the performance at a rate of 0.28% per sol
#           during the initial 30 sols of the mission.
#       2.2 Longer measures on MER missions indicate that long-term degradation
#            is about 0.14% per sol.
#
#   3. Jeremiah McNatt et al.:
#       After deployment, a 5% permanent dust power loss is added to the assumption
#       with more accumulated dust removed periodically.
#
#   4. Paul M. Stell et al.
#       Dust performance degradation is about 30%.
#
PR = 1 - (0.03 + 0.05 + 0.30)

# FIXME: During storm, we get different values if nfft=1 or nfft=2
Gh_worst = function(Ls_range, Z, tau, al=0.1, nfft=2, verbose=TRUE){
  Gh_worst = 600
  Ls_worst = -1
  
  for(Ls in Ls_range){
    Gh = Gh_eq(Ls, Z=Z, tau=tau, al=al, nfft=nfft) 
    if(Gh < Gh_worst){
      Gh_worst = Gh
      Ls_worst = Ls
    }
  }
  
  if(isTRUE(verbose)){
    text_template = "Worst high-noon irradiance with tau = {{tau}} occurs at Ls = {{Ls}}° with Gh = {{Gh}} W/m2."
    text_data = list(tau=tau, Ls=Ls_worst, Gh=round(Gh_worst))
    text = whisker.render(text_template, text_data)
    print(text)
  }
  
  return(list(
    "Ls"=Ls_worst,
    "Gh"=Gh_worst,
    "tau" = tau))
}

Hh_worst = function(Ls_range, phi, tau, al=0.1, nfft=3, verbose=TRUE){
  Hh_worst = 50000
  Ls_worst = -1
  
  for(Ls in Ls_range){
    Hh = Hh_eq(Ls, phi, tau, al=al, nfft=nfft)
    
    if(Hh < Hh_worst){
      Hh_worst = Hh
      Ls_worst = Ls
    }
  }
  
  if(isTRUE(verbose)){
    text_template = "Worst daily insolation with tau = {{tau}} occurs at Ls = {{Ls}}° with Hh = {{Hh}} W/m2."
    text_data = list(tau=tau, Ls=Ls_worst, Hh=round(Hh_worst))
    text = whisker.render(text_template, text_data)
    print(text)
  }
  
  return(list(
    "Ls" = Ls_worst,
    "Hh" = Hh_worst,
    "tau" = tau))
}

# Get the worst moment during a year without global storm
tau = 0.5

print('During a year with no global storms:')
Hh_w1 = Hh_worst(Ls_range=1:360, phi, tau)
Gh_w1 = Gh_worst(Ls_range=1:360, Z=0, tau=tau)

print("")

# Get worst moment during global storm season.
tau = 5 # Jeremiah McNatt et al.

print('During global storms season:')
Hh_w2 = Hh_worst(Ls_range=Ls_global_dust_storm_season, phi, tau)
Gh_w2 = Gh_worst(Ls_range=Ls_global_dust_storm_season, Z=0, tau=tau)

Hh_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$Hh else Hh_w2$Hh
Ls_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$Ls else Hh_w2$Ls
tau_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$tau else Hh_w2$tau

# Total solar panel area [m2].
A = 3

Ts_start = sunrise(Ls_w, phi, unit=3)
Ts_end = sunset(Ls_w, phi, unit=3)

Ts_range = Ts_start:Ts_end
data_matrix = matrix(NA, nrow=4, ncol=length(Ts_range))
rownames(data_matrix) = c('Ts', 'Z', 'Gh', 'E')

Gh = Gh_eq(Ls_w, tau=0.5, al=0.1, T_s=12, phi=5, nfft=3) 
print(Gh)

# Ts_index = 1
# for(Ts in Ts_range){
#   Z = Z_eq(Ls_w, Ts, phi, nfft=3)
#   
#   # Irradiance [W/m2].
#   Gh = Gh_eq(Ls_w, Z=Z, tau=tau_w, al=0.1, nfft=3) 
#   
#   # Energy generated from solar panels [Wh].
#   E = A * e * Gh * PR
#   
#   #FIXME: First Gh is negative
#   data_matrix[1, Ts_index] = Ts
#   data_matrix[2, Ts_index] = Z
#   data_matrix[3, Ts_index] = Gh
#   data_matrix[4, Ts_index] = E
#   
#   Ts_index = Ts_index + 1
# }
# 
# dev.new()
# plot(round(Ts_range), data_matrix['E',],
#      xlab="Solar Time, T [h]",
#      ylab="Energy, E [Wh]",
#      type="l",
#      font.sub=2,
#      cex.sub=1.2)

#print(data_matrix)

# Get sunrise time.
# Get sunset time.