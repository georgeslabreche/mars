

library("here")
library("plot3D") # Needed for scatter3D
library(rgl)
library(wesanderson)

# Plot function.
tau_eq = dget(here("functions", "tau.R"))

phi_seq = seq(-90, 90, 5) # [deg]
Ls_seq = seq(150, 360, 5) # [deg]
models = c(1, 2)


# Using the outer() function doesn't work with the tau_eq equation:
outer_matrix_persp3D = function(phi_seq, Ls_seq, model){
  tau_vect = c()
  
  for(Ls in Ls_seq){
    for(phi in phi_seq){
      tau_vect = c(tau_vect, tau_eq(phi, Ls, model))
    }
  }
  
  # 3. Build the outer matrix.
  matrix(tau_vect, nrow=length(phi_seq))
}

outer_matrix_persp = function(phi_seq, Ls_seq, model){
  tau_vect = c()
  
  for(phi in phi_seq){
    for(Ls in Ls_seq){
      tau_vect = c(tau_vect, tau_eq(phi, Ls, model))
    }
  }
  
  # 3. Build the outer matrix.
  matrix(tau_vect, nrow=length(Ls_seq))
}

# Plot the results

######################################################################################
# Perspective: Variation of optical depth with latitudes and areocentric longitudes. #
######################################################################################
matrix_persp_1 = outer_matrix_persp(phi_seq, Ls_seq, 1)

dev.new()
par(mfrow=c(1,2))
persp(x=Ls_seq, y=phi_seq, z=matrix_persp_1,
      xlab="Ls", ylab="φ", zlab="τ",
      ticktype="simple",
      zlim=c(0.5, 8),
      theta=150,
      phi=25,
      col=wes_palette("Zissou1", 5)[2],
      font.sub=2,
      cex.sub=1.2,
      sub="Model 1")


matrix_persp_2 = outer_matrix_persp(phi_seq, Ls_seq, 2)
persp(x=Ls_seq, y=phi_seq, z=matrix_persp_2,
      xlab="Ls", ylab="φ", zlab="τ",
      ticktype="simple",
      zlim=c(0.5, 8),
      theta=150,
      phi=25,
      col=wes_palette("Zissou1", 5)[3],
      font.sub=2,
      cex.sub=1.2,
      sub="Model 2")

mtext("Variation of optical depth with latitudes and areocentric longitudes", side=3, line=-3, outer=TRUE)


#########################################################################################
# Perspective 3D: Variation of optical depth with latitudes and areocentric longitudes. #
#########################################################################################

open3d()

matrix_persp3D_1 = outer_matrix_persp3D(phi_seq, Ls_seq, 1)
mfrow3d(1, 2, sharedMouse = TRUE)
persp3d(x=phi_seq, y=Ls_seq, z=matrix_persp3D_1,
        xlab="φ",
        ylab="Ls",
        zlab="τ",
        zlim=c(0.5, 8),
        col=wes_palette("Zissou1", 5)[2],)

matrix_persp3D_2 = outer_matrix_persp3D(phi_seq, Ls_seq, 2)
persp3d(x=phi_seq, y=Ls_seq, z=matrix_persp3D_2,
        xlab="φ",
        ylab="Ls",
        zlab="τ",
        zlim=c(0.5, 8),
        col=wes_palette("Zissou1", 5)[3])
