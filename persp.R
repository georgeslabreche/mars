# Future work - persp3d plot may not be necessary for this....
# 2 possible worst cases:
#   A. Furtherest away from the sun: at periphelion.
#   B. Furthest away from the sun during dust season at Vernal Equinox (Ls=0) - Dust Storm Season ends.
# Check distance from sun during Vernal and Autumn equinox. Should be the same. However we generate more power during Autumn. Why?
# Color the areas in the plane where we expect the rover to still work or die.

library("plot3D") # Needed for scatter3D
library(rgl)
# When attempting to install rgl on Ubuntu 18.04.2 LTS, the following error was thrown:
# ext/ftgl/FTGL/ftgl.h:32:10: fatal error: ft2build.h: No such file or directory
# This was resolved by installing the following beforehard:
# sudo apt-get install libfreetype6-dev
# Reference and solutions for other operating systems can be found here: 
# https://stackoverflow.com/questions/31820865/error-in-installing-rgl-package

Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Storm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE,  Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

e = 0.093377    # Mars orbit eccentricity.
Mb = 590        # Mean beam irradiance at the top of the Martian atmosphere


nnff = read.csv("normalized-net-flux-function-at-martian-surface.csv")
rownames(nnff) = sprintf("%1.2f", nnff[,1])
nnff = nnff[-c(1)]

zenith_angles = as.numeric(gsub("X", "", colnames(nnff)))
taus_all = as.numeric(rownames(nnff))
taus_selected = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)

# Which taus vector to work with.
taus = taus_all

Gob_eq = function(Ls){
  Mb * ( (1 + e*cos( (Ls-Ls_P)* pi/180 ))^2 / (1-e^2)^2 ) # Eq. 4.
}

f = function(Z, tau){
  nnff[sprintf("%1.2f", tau), paste("X", Z, sep="")]
}

# Global irradiance on Mars horizontal surface.
Gh_eq = function(Ls, Z, tau){
  Gob_eq(Ls) * cos(Z * pi/180) * (f(Z, tau) / 0.9)
}


# Using the outer() function doesn't work with the Gh_eq equation:
# z=outer(zenith_angles, taus, Gh_eq)
# So we write our own function.
outer_matrix = function(Ls, zenith_angles, taus){
  # 1. Build first row of the outer vector
  outer_vector = Gh_eq(Ls, zenith_angles, taus[1])
  
  # 2. Build the rest and concatenate to the vector, we will split in the end.
  for(tau in tail(taus, -1)){
    row = Gh_eq(Ls, zenith_angles, tau)
    outer_vector = c(outer_vector, row)
  }
  # 3. Build the outer matrix.
  matrix(outer_vector, ncol=length(zenith_angles))
}


# Plot the results

# For worst case A:
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



