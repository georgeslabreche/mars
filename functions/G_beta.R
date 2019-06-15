# Equation 3 (1994): Global irradiance on an inclined surface [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis & Norambuena, Marcos. (1994). Solar radiation on Mars: Tracking photovoltaic array. Journal of Propulsion and Power. 12. 10.2514/3.24044 
#   https://www.researchgate.net/publication/24286713_Solar_radiation_on_Mars_Tracking_photovoltaic_array
library(here)

# Table I (Update 1991): The albedo function.
albedo = dget(here("functions", "al.R"))

# Equation 4 (1990): Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Equation 6 (1990): Zenith angle of the incident solar radiation [deg].
Z_eq = dget(here("functions", "Z.R"))

# Equation 14 (1990): Beam irradiance on Mars surface [W/m2].
Gb_eq = dget(here("functions", "G_b.R"))

# Equation 16 (1990): The solar irradiance components on a horizontal Martian surface [W/m2].
Gdh_eq = dget(here("functions", "G_dh.R"))

# Equation 17 (1990): Global irradiance on Mars horizontal surface [W/m2].
Gh_eq = dget(here("functions", "G_h.R"))

# Mars obliquity of rotation axis [W/m2].
delta_0 = 24.936

# Equation 3 (1994): Global irradiance on an inclined surface.
#
#   Ls        - Areocentric longitude [deg].
#   omega     - Hour angle value [h].
#                 - An integer belonging to [6, 18] if using net flux function f_89 or f_90 (nfft 1 or 2).
#                 - An interger belonging to [0, 24] if using net flux function f_analytical (nfft 3).
#   phi       - Latitude [deg].
#   tau       - Optical depth.
#   al        - Albedo
#   beta      - Slope/Tilt angle [deg].
#   gamma_c   - Sun surface azimuth angle (i.e. orientation angle) [deg].
#   nfft      - Net flux function implementation type.
#                 - 1 for f_89.
#                 - 2 for f_90.
#                 - 3 for f_analytical.
test = function(Ls, omega, phi, tau, al, beta, gamma_c, nfft){
  
  # Equation 7 (1990): Declination angle [rad].
  delta = asin(sin(delta_0 * pi/180) * sin(Ls * pi/180))
  
  # Equation 8 (1990): Hour angle [deg].
  # From Appelbaum, Joseph & Flood, Dennis. (1990):
  #   The ratio of Mars to Earth length of day is 24.65/24.
  #   It is convenient, for calculation purposes, to define a Mar hour
  #   by dividing the Martian day into 24 hr. Using the same relationship
  #   between the Mars solar time T and the hour angle as for the Earth.
  omega_deg = 15 * omega - 180
  
  Z = Z_eq(Ls, omega, phi, nfft)

  #x = sin(delta) - cos(Z * pi/180) * sin(phi * pi/180)
  #y = sin(Z * pi/180) * cos(phi * pi/180)
  #gamma_s = acos(x / y)
  #print(gamma_s * 180/pi)
  
  # Angle of solar elevation [deg]
  m = sin(delta) * sin(phi * pi/180)
  n = cos(delta) * cos(omega_deg * (pi/180)) * cos(phi * pi/180)
  alpha = asin(m + n)
  
  # Solar Azimuth Angle [deg]
  # Source: http://mypages.iit.edu/~maslanka/SolarGeo.pdf
  # x = sin(delta) * cos(phi * pi/180)
  # y = cos(delta) * cos(omega_deg * (pi/180)) * sin(phi * pi/180)
  # z = cos(alpha)
  # 
  # gamma_s_prime = acos((x - y) / z) # [rad]
  # gamma_s = if(omega_deg <= 0) (gamma_s_prime * 180/pi) else (360 - (gamma_s_prime * 180/pi)) # [deg]
  
  # Solar Azimuth Angle [deg]
  # Source: https://www.giss.nasa.gov/tools/mars24/help/algorithm.html
  print(paste("omega_deg:", omega_deg))
  
  # THIS DOES NOT MATTER WHEN BETA = 0 because it leads to j=0
  gamma_s2 = atan(sin(omega_deg * (pi/180)) / ((cos(phi * pi/180) * tan(delta)) - (sin(phi * pi/180) * cos(omega_deg * (pi/180))))) # [rad]
  gamma_s2 = gamma_s2 * 180/pi # [deg]
  gamma_s = gamma_s2  # [deg]
  
  #gamma_s = 151.93895
  
  # Equation 4 (1994): Sun Angle of Incidence [VERIFIED]
  i = cos(beta * pi/180) * cos(Z * pi/180)
  j = sin(beta * pi/180) * sin(Z * pi/180) * cos((gamma_s - gamma_c) * pi/180) # THIS DOES NOT MATTER WHEN BETA = 0 because it leads to j=0
  teta = acos(i + j)
  
  
  #print(paste("Sun Azimuth Angle Prime:", gamma_s_prime * 180/pi))
  #print(paste("Sun Azimuth Angle:", gamma_s))
  print(paste("Sun Azimuth Angle [doesn't matter]:", gamma_s)) # THIS DOES NOT MATTER WHEN BETA = 0 because it leads to j=0
  print(paste("Sun Angle of Incidence:", teta * 180/pi))
  print(paste("Sun Zenith Angle:", Z))

  a = Gb_eq(Ls, Z, tau) * cos(teta)  
  b = Gdh_eq(Ls, Z, tau, al, nfft) * cos((beta * pi/180) / 2)^2
  c = al * Gh_eq(Ls, Z, tau, al, nfft) * sin((beta * pi/180) / 2)^2 # THIS DOES NOT MATTER WHEN BETA = 0 because it equals 0
  
  result = a + b + c
  
  return(result)
}

# Verification Source: https://www.giss.nasa.gov/tools/mars24/help/algorithm.html
#Ls = 277.18758
#omega = 23.99425
#phi = 0

Ls = 327.32416
omega = 13.16537
phi = -14.640
#phi = 10 

tau = 0.5
al = 0.1
# If using the albedo function, the longitude and latitude (phi) must be a multiple of 10.
#l = albedo(0, phi)
beta = 0
gamma_c = 0 # The rover is oriented soutwards [deg].
nfft = 3

G_beta = test(Ls, omega, phi, tau, al, beta, gamma_c, nfft)
print(paste("Gβ:", G_beta))

Z = Z_eq(Ls, omega, phi, nfft)
Gh = Gh_eq(Ls, Z, tau, al, nfft)
print(paste("Gh:", Gh))


# Equation 14 (1994): Sun angle of incidence [rad].
#teta = acos(1 - cos(delta)^2 * cos(omega_deg * (pi/180)))^(1/2)
#beta = Z

# Equation 16 (1994): Sun angle of incidence [rad].
# East-West horizontal axis: North-South tracking.
# 
#x = sin(delta) * sin((phi-beta) * (pi/180))
#y = cos(delta) * cos((phi-beta) * (pi/180)) * cos(omega_deg * (pi/180))
#teta = acos(x + y)

# (b) North-South horizontal axis: East-West tracking.

