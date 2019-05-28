  # Load
  library(wesanderson)
  
  # Areocentric Longitude values (deg).
  Ls_VE = 0       # Vernal Equinox.
  Ls_A = 71       # APHELION.
  Ls_SS = 90      # Summer Solstice.
  Ls_AE = 180     # Autumn Equinox.
  Ls_P = 248      # PERIPHELION.
  Ls_WS = 270     # Winter Solstice.
  Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE,  Ls_P, Ls_WS)
  Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')
  
  e = 0.093377    # Mars orbit eccentricity.
  Mb = 590        # Mean beam irradiance at the top of the Martian atmosphere
  
  
  ###
  # Equation 4: Beam irridiance at the top of the Martian atmosphere (W/m2).
  ###
  
  # Beam irridiance at the top of the Martian atmosphere (W/m2). 
  Gob_eq = function(Ls){
    Mb * ( (1 + e*cos( (Ls-Ls_P)* pi/180 ))^2 / (1-e^2)^2 ) # Eq. 4.
  }
  
  dev.new()
  curve(Gob_eq, 0, 360, 360,
    ylab="Beam irradiance at top of Mars atmosphere [W/m2]",
    xlab="Areocentric Longitude [deg]",
    xaxt='n',
    yaxt='n',
    ylim=c(450, 750),
    type="l",
    col="blue")
  axis(1, seq(0, 360, 60))
  axis(2, seq(450, 800, 50))

  points(Ls_seq, ceiling(Gob_eq(Ls_seq)),
         pch="+",
         col="red")

  for(Ls in Ls_seq){
    lines(c(Ls, Ls), c(0, ceiling(Gob_eq(Ls))),
          lty=2,
          col="red")
    lines(c(-15, Ls), c(ceiling(Gob_eq(Ls)), ceiling(Gob_eq(Ls))),
          lty=2,
          col="red")
  }

  text(Ls_seq, Gob_eq(Ls_seq),
       labels=paste(Ls_lbl_seq, " (", ceiling(Gob_eq(Ls_seq)), " W/m²)", sep=""),
       cex=0.7,
       pos=4)
  
  
  ###
  # Equation 17: Global irradiance on Mars horizontal surface (W/m2).
  ###
  nnff = read.csv("normalized-net-flux-function-at-martian-surface.csv")
  rownames(nnff) = sprintf("%1.2f", nnff[,1]) 
  nnff = nnff[-c(1)]
  
  #taus = as.numeric(rownames(nnff))
  taus = c(0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0)
  zenith_angles = as.numeric(gsub("X", "", colnames(nnff)))
  
  f = function(Z, tau){
    nnff[sprintf("%1.2f", tau), paste("X", Z, sep="")]
  }
  
  # Global irradiance on Mars horizontal surface.
  Gh_eq = function(Ls, Z, tau){
    Gob_eq(Ls) * cos(Z * pi/180) * (f(Z,tau) / 0.9)
  }
  

  ###
  # Title: Variation of global irradiance on Mars horizontal for different optical depths.
  # Subtitle: Effect of areocentric longitude with sun zenith angle as a parameter.
  ###
  dev.new()
  par(mfrow=c(4,3))
  for(tau in taus){
    color_index = 1

    for(Z in zenith_angles){
      curve(Gh_eq(x, Z, tau), 0, 360, 360,
            add=ifelse(color_index>1, TRUE, FALSE),
            ylim=c(0, 700),
            xlab="Ls [deg]", ylab="Gh [W/m2]",
            sub=paste("τ = ", tau, sep=""),
            font.sub=2,
            cex.sub=1.2,
            type="l",
            col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[color_index])

      color_index = color_index + 1
    }
  }

  mtext("Variation of global irradiance on Mars horizontal surface for different optical depths\nEffect of areocentric longitude with sun zenith angle as a parameter", side = 3, line = -3, outer = TRUE)

  # Add a legend
  plot.new()
  legend("top", title="Zenith Angle",
         paste("Z = ", zenith_angles, "°", sep=""),
         col = wes_palette("FantasticFox1", length(zenith_angles), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  
  ###
  # Title: Variation of global irradiance on Mars horizontal for different sun zenith angles.
  # Subtitle: Effect of areocentric longitude with optical depth as a parameter.
  ###
  dev.new()
  par(mfrow=c(4,3))
  for(Z in rev(zenith_angles)){
    color_index = 1

    for(tau in taus){
      curve(Gh_eq(x, Z, tau), 0, 360, 360,
            add=ifelse(color_index>1, TRUE, FALSE),
            ylim=c(0, 700),
            xlab="Ls [deg]", ylab="Gh [W/m2]",
            sub=paste("Z = ", Z, "°", sep=""),
            font.sub=2,
            cex.sub=1.2,
            type="l",
            col=wes_palette("FantasticFox1", length(taus), type = "continuous")[color_index])
      color_index = color_index + 1
    }
  }

  plot.new()
  legend("topleft", title="Optical Depth",
         paste("τ = ", taus, sep=""),
         col=wes_palette("FantasticFox1", length(taus), type = "continuous"),
         cex=0.8, bty="n", lty=1)

  mtext("Variation of global irradiance on Mars horizontal surface for different solar zenith angles\nEffect of areocentric longitude with optical depth as a parameter", side = 3, line = -3, outer = TRUE)

  
  ###
  # Title: Variation of global irradiance on Mars horizontal surface for different areocentric longitudes.
  # Subtitle: Effect of sun zenith angle with optical depth as a parameter.
  ###
  dev.new()
  par(mfrow=c(3,3))
  for(Ls in Ls_seq){
    index = 1
    for(tau in taus){
      if(index == 1){
        plot(zenith_angles, Gh_eq(Ls, zenith_angles, tau),
             ylim=c(0, 700),
             xlab="Z [deg]", ylab="Gh [W/m2]",
             sub=paste("Ls = ", Ls, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             col=wes_palette("FantasticFox1", length(taus), type = "continuous")[index])
      }else{
        lines(zenith_angles, Gh_eq(Ls, zenith_angles, tau),
              sub=paste("Ls = ", Ls, "°", sep=""),
              font.sub=2,
              cex.sub=1.2,
              type="l",
              col=wes_palette("FantasticFox1", length(taus), type = "continuous")[index])
      }
      index = index + 1
    }
  }

  plot.new()
  legend("top", title="Optical Depth",
         paste("τ = ", taus, sep=""),
         col=wes_palette("FantasticFox1", length(taus), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  mtext("Variation of global irradiance on Mars horizontal surface for different areocentric longitudes\nEffect of sun zenith angle with optical depth as a parameter", side = 3, line = -3, outer = TRUE)
  
  ###
  # Title: Variation of global irradiance on Mars horizontal surface for different areocentric latitudes.
  # Subtitle: Effect of optical depth with sun zenith angle as a parameter.
  ###
  dev.new()
  par(mfrow=c(3,3))
  Ls = 180
  for(Ls in Ls_seq){
    index = 1
    for(zenith_angle in zenith_angles){
      if(index == 1){
        plot(taus, Gh_eq(Ls, zenith_angle, taus),
             ylim=c(0, 700),
             xlab="τ", ylab="Gh [W/m2]",
             sub=paste("Ls = ", Ls, "°", sep=""),
             font.sub=2,
             cex.sub=1.2,
             type="l",
             col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[index])
      }else{
        lines(taus, Gh_eq(Ls, zenith_angle, taus),
              sub=paste("Ls = ", Ls, "°", sep=""),
              font.sub=2,
              cex.sub=1.2,
              type="l",
              col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous")[index])
      }
      index = index + 1
    }
  }

  plot.new()
  legend("top", title="Zenith Angle",
         paste("Z = ", zenith_angles, "°" , sep=""),
         col=wes_palette("FantasticFox1", length(zenith_angles), type = "continuous"),
         cex=0.8, bty="n", lty=1)
  mtext("Variation of global irradiance on Mars horizontal surface for different areocentric longitudes\nEffect of optical depth with sun zenith angle as a parameter", side = 3, line = -3, outer = TRUE)

