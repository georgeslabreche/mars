library(configr)
library(here)
library(slugify)
library(RColorBrewer)

config_filepath = here('main', 'config.yml')
config = read.config(file=config_filepath)

Gobh_eq = dget(here("functions", "G_obh.R"))

phi = -2
nfft = 3
Ls_seq = c(153, 120, 69, 249, 299)

# Color blind friendly colors.
cols = brewer.pal(n=length(Ls_seq), name="PuOr")

dev.new()
par(bg='white')
Ls_index = 1
for(Ls in Ls_seq){
  Gobh_func = function(x){
    Gobh_eq(Ls=Ls, phi=phi, T_s=x, nfft=nfft)
  }
  

  plot(Gobh_func, 12, 19, add=(Ls != Ls_seq[1]),
       xlab="", ylab="",
       ylim=c(0, 700),
       lwd=4,
       col=cols[Ls_index])

  
  Ls_index = Ls_index + 1
}

legend("topright", title="Ls", legend=paste(Ls_seq, "°", sep=""),
       col=cols, lty=1, lwd=4, cex=1)

plot_title = "Diurnal variation of beam irradiance on a\nhorizontal surface at top of Mars atmosphere"
title(main=plot_title,
      xlab="Solar Time [hr]",
      ylab="Beam Irradiance, Gobh [W/m2]")

plot_filepath = paste(config$output$marsenv, slugify(plot_title, space_replace="-"), ".png", sep="")
dev.copy(png, plot_filepath)
dev.off()

build_latex_plot_figure = dget(here("main/thesis", "helpers.R"))
build_latex_plot_figure(plot_title, plot_filepath)

