# Curve for Equation 4: Beam irridiance at the top of the Martian atmosphere [W/m2].
#
# Based on equations presented in the following publication:
#   Appelbaum, Joseph & Flood, Dennis. (1990). Solar radiation on Mars. Solar Energy. 45. 353–363. 10.1016/0038-092X(90)90156-7. 
#   https://www.researchgate.net/publication/256334925_Solar_radiation_on_Mars

# Load
library(here)
library(configr)
library(slugify)
library(RColorBrewer)

config_filepath = here('main', 'config.yml')
config = read.config(file=config_filepath)

# Equation 4: Beam irridiance at the top of the Martian atmosphere [W/m2].
Gob_eq = dget(here("functions", "G_ob.R"))

# Areocentric Longitude values (deg).
Ls_VE = 0       # Vernal Equinox - Dust Storm Season ends.
Ls_A = 71       # APHELION.
Ls_SS = 90      # Summer Solstice.
Ls_AE = 180     # Autumn Equinox - Dust Storm Season begins.
Ls_P = 248      # PERIPHELION - Dust Storm Season.
Ls_WS = 270     # Winter Solstice - Dust Storm Season.
Ls_seq = c(Ls_VE, Ls_A, Ls_SS, Ls_AE,  Ls_P, Ls_WS)
Ls_lbl_seq = c('Vernal Equinox', 'Aphelion', 'Summer Solstice', 'Autumn Equinox', 'Periphelion', 'Winter Solstice')

# Color blind friendly colors.
cols = brewer.pal(n=3, name="PuOr")

dev.new()
par(bg='white')
curve(Gob_eq, 0, 360, 360,
  ylab="Irradiance [W/m2]",
  xlab="Areocentric Longitude [deg]",
  xaxt='n',
  yaxt='n',
  ylim=c(450, 750),
  type="l",
  lwd=5,
  col=cols[1])
axis(1, seq(0, 360, 60))
axis(2, seq(450, 800, 50))

points(Ls_seq, ceiling(Gob_eq(Ls_seq)),
       pch="+",
       col=cols[3])

for(Ls in Ls_seq){
  lines(c(Ls, Ls), c(0, ceiling(Gob_eq(Ls))),
        lty=2,
        col=cols[3])
  lines(c(-15, Ls), c(ceiling(Gob_eq(Ls)), ceiling(Gob_eq(Ls))),
        lty=2,
        col=cols[3])
}

text(Ls_seq, Gob_eq(Ls_seq),
     labels=paste(Ls_lbl_seq, " (", ceiling(Gob_eq(Ls_seq)), " W/m²)", sep=""),
     cex=0.7,
     pos=4)

plot_title = "Beam irradiance at top of Mars atmosphere\nas a function of Areocentric Longitude"
title(main=plot_title)

dev.copy(png, paste(config$output$marsenv, slugify(plot_title, space_replace="-"), ".png", sep=""))
dev.off()