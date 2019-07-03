# InSight generated 4,588 watt-hours on its first sol.
# Phoenix lander generated around 1,800 watt-hours in a day.

library(here)
library(whisker)
library(wesanderson)

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Global daily insolation on Mars horizontal surface [Wh/m2-day].
Hh_eq = dget(here("functions", "H_h.R"))

# Equation 6: Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Daylight range.
daylight_range = dget(here("utils", "daylight_range.R"))

E_cols = wes_palette("Darjeeling1", 4)

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
Gh_worst = function(Ls_range, Z, tau, al=0.1, nfft=3, verbose=FALSE){
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

Hh_worst = function(Ls_range, phi, tau, al=0.1, nfft=3, verbose=FALSE){
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

get_worst_irradiance = function(A, Ls_range, phi, tau, nfft=3, verbose=FALSE){
  # Get the worst insolation.
  Hh_w = Hh_worst(Ls_range=Ls_range, phi=phi, tau=tau, nfft=nfft, verbose=verbose)
  
  # Get the daylight solar time range for the Ls with the worst insolation.
  Ts_range = daylight_range(Ls=Hh_w$Ls, phi=phi, T_step=1)
  
  # Prep data matrix.
  data_matrix = matrix(NA, nrow=4, ncol=length(Ts_range))
  rownames(data_matrix) = c('Ts', 'Z', 'Gh', 'E')
  
  # Calculate diurnal values for Z, Gh, and E.
  # TODO: Eventually calculate solar panel size for desired E and figure out cut off point
  
  Ts_index = 1
  for(T_s in Ts_range){
    Gh = Gh_eq(Ls=Hh_w$Ls, phi=phi, T_s=T_s, tau=tau, al=0.1, nfft=nfft)
    Z = Z_eq(Ls=Hh_w$Ls, T_s=T_s, phi=phi, nfft=nfft)
    
    # Energy generated from solar panels [Wh].
    E = A * e * Gh * PR
    
    #FIXME: First Gh is negative
    data_matrix[1, Ts_index] = T_s
    data_matrix[2, Ts_index] = Z
    data_matrix[3, Ts_index] = Gh
    data_matrix[4, Ts_index] = E
    
    Ts_index = Ts_index + 1
  }
  
  return(data_matrix)
}

# Get the worst moment during a year without global storm
tau = 0.5

#print('During a year with no global storms:')
#Hh_w1 = Hh_worst(Ls_range=1:360, phi, tau)
#Gh_w1 = Gh_worst(Ls_range=1:360, Z=0, tau=tau)

#print("")

# Get worst moment during global storm season.
#tau = 5 # Jeremiah McNatt et al.

print('During global storms season:')
#Hh_w2 = Hh_worst(Ls_range=Ls_global_dust_storm_season, phi, tau)
#Gh_w2 = Gh_worst(Ls_range=Ls_global_dust_storm_season, Z=0, tau=tau)

#Hh_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$Hh else Hh_w2$Hh
#Ls_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$Ls else Hh_w2$Ls
#tau_w = if(Hh_w1$Hh < Hh_w2$Hh) Hh_w1$tau else Hh_w2$tau

# Total solar panel area [m2].
A = 1

###############################
# Without global dust storms. #
###############################
Ls_range = 1:360
tau = 1
data_matrix = get_worst_irradiance(A, Ls_range, phi, tau, nfft=3, verbose=TRUE)

# PLot
dev.new()
plot(round(data_matrix['Ts',]), data_matrix['E',],
     xlab="Solar Time, T [h]",
     ylab="Energy, E [Wh]",
     type="l",
     col=E_cols[1],
     font.sub=2,
     cex.sub=1.2)


############################
# With global dust storms. #
############################
Ls_range = Ls_global_dust_storm_season
tau = 5
data_matrix = get_worst_irradiance(A, Ls_range, phi, tau, nfft=3, verbose=TRUE)

lines(round(data_matrix['Ts',]), data_matrix['E',],
      col=E_cols[2])

#print(data_matrix)

# Get sunrise time.
# Get sunset time.