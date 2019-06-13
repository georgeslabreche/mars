# Generate 3D perspective plots describing solar radiation on Mars as a function of solar zenith angle and atmospheric opacity.
#
# Based on equations presented in the following publication:
# Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
# https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Future work - persp3d plot may not be necessary for this....
# 2 possible worst cases:
#   A. Furtherest away from the sun: at periphelion.
#   B. Furthest away from the sun during dust season at Vernal Equinox (Ls=0) - Dust Storm Season ends.
# Check distance from sun during Vernal and Autumn equinox. Should be the same. However we generate more power during Autumn. Why?
# Color the areas in the plane where we expect the rover to still work or die.
# How about diffuse and beam irradiance?

library("here")
library("plot3D") # Needed for scatter3D
library(rgl)
# When attempting to install rgl on Ubuntu 18.04.2 LTS, the following error was thrown:
# ext/ftgl/FTGL/ftgl.h:32:10: fatal error: ft2build.h: No such file or directory
# This was resolved by installing the following beforehard:
# sudo apt-get install libfreetype6-dev
# Reference and solutions for other operating systems can be found here: 
# https://stackoverflow.com/questions/31820865/error-in-installing-rgl-package

# Equation 17: Global irradiance on Mars horizontal surface (W/m2).
Gh_eq = dget(here("functions", "G_h.R"))

# The normalized net flux function.
# FIXME: Edit this to grab based on nfft.
f_all_taus = dget(here("functions", "f_all_taus.R"))
f_all_Zs = dget(here("functions", "f_all_Zs.R"))

al = 0.1    # Albedo.
nfft = 2    # Net flux function type (1 for f_89, 2 for f_90, and 3 for f).
Ls_VE = 0   # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71   # APHELION.

# Tau list options
taus_all = f_all_taus(nfft)
taus_clear_day = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
taus_selected = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)

# Which taus vector to work with.
taus = taus_selected

# Zenith angle options
zenith_angles = f_all_Zs(nfft)

# Using the outer() function doesn't work with the Gh_eq equation:
# z=outer(zenith_angles, taus, Gh_eq)
# So we write our own function.
outer_matrix = function(Ls, zenith_angles, taus){
  # 1. Build first row of the outer vector
  outer_vector = Gh_eq(Ls, zenith_angles, taus[1], al, nfft)
  
  # 2. Build the rest and concatenate to the vector, we will split in the end.
  for(tau in tail(taus, -1)){
    row = Gh_eq(Ls, zenith_angles, tau, al, nfft)
    outer_vector = c(outer_vector, row)
  }
  # 3. Build the outer matrix.
  matrix(outer_vector, ncol=length(zenith_angles))
}

# Plot the results

# For worst case A :
matrix = outer_matrix(Ls_A, zenith_angles, taus)
open3d()
persp3d(x=zenith_angles, y=taus, z=matrix,
      xlab="Z", ylab="τ", zlab="Gh",
      col = "aquamarine",
      ticktype="detailed")
surface3d(x=zenith_angles, y=taus, z=matrix, back = "lines")
surface3d(x=zenith_angles, y=taus, z=matrix, front = "lines")

# For case B:
matrix_VE = outer_matrix(Ls_VE, zenith_angles, taus)
open3d()
persp3d(x=zenith_angles, y=taus, z=matrix_VE,
        xlab="Z", ylab="τ", zlab="Gh",
        col = "coral1",
        ticktype="detailed")
surface3d(x=zenith_angles, y=taus, z=matrix_VE, back = "lines")
surface3d(x=zenith_angles, y=taus, z=matrix_VE, front = "lines")




