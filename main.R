# Areocentric Longitude values (deg).
Ls_P = 248      # Periphelion. Apple say 248
Ls_A = 71       # Aphelion.
Ls_VE = 0       # Vernal Equinox.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox
Ls_WS = 270     # Winter Solstice.

S = 1371        # The solar constant at the mean Sun-Earth distance of 1 AU (W/m2).
a = 1.524       # Mars semimajor axis (AU).

e = 0.093377    # Mars orbit eccentricity.
Ls = 25        # Areocentric Longitude (deg).
                #   Aphelion, largest Sun-Mars distance, at Ls=72.
                #   Perihelion, smallest Sun-Mars distance, at Ls=251
teta = Ls-Ls_P  # True anomaly is given by Ls - 251 deg.
                # 251 deg is the areocentric longitude of Mars periphelion.
r = NULL        # Instanteneous Sun-Mars distance (AU).
Z = NULL        # Zenith angle (deg).
tau = NULL      # Tau factor for atmospheric opacity.

####
# Radiation Values
####
Gob = NULL      # Beam irradiance at the top of Mars atmosphere.

# The solar irradiance components, on a horizontal Martian surface,
# are related by Gh = Gbh + Gdh.
Gh = NULL       # Global irradiance on Mars horizontal surface (W/m2).
Gbh = NULL      # Direct beam irradiance on Mars horizontal surface (W/m2).
Gdh = NULL      # Diffuse irradiance on Mars horizontal surface(W/m2).

# Instanteneous Sun-Mars distance (AU).
r = (a*(1-e^2)) / (1 + e * cos(teta)) # Eq. 2.

# Beam irridiance at the top of the Martian atmosphere (W/m2). 
Gob = S / r^2 # Eq. 1.         

# Beam irradiance at the top of Mars atmosphere.
# Value
# Gob = Mb * ((1 + e*cos(teta))^2 / (1-e^2)^2)
# 
# # Equation.
Gob_eq = function(x){
  590 * ( (1 + e*cos(x-Ls_P))^2 / (1-e^2)^2 )
}

# Gob_eq = function(x){
#   S / ((a*(1-e^2)) / (1 + e * cos(x-Ls_P)))^2
# }

# Plot Beam irradiance at top of Mars atmosphere [W/m2] as a function of Areocentric Longitude [deg].
Ls_seq = seq(0, 360, 1)
#x <- c(0, 69, 90, 180, 248, 270, 360)
x <- c(0, 30, 69, 90, 180, 248, 270, 360)
plot(Ls_seq,
     Gob_eq(Ls_seq),
     ylab="Beam irradiance at top of Mars atmosphere [W/m2]",
     xlab="Areocentric Longitude [deg]",
     type="l",
     col="blue")

#Gh = Gob * cos(Z)