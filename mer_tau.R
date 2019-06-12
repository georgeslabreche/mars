# Phoenix
#   Dataset:      PHOENIX Atmospheric Opacity data.
#   Desctiption:  Derived data from the SSI instrument measuring the atmospheric opacity for sols 1-151.
#   Source:       PDS - Planetary Atmospheres Node
#   URL:          https://pds-atmospheres.nmsu.edu/data_and_services/atmospheres_data/Mars/Mars.html
#
# Viking Landers
#   Dataset:      VL1/VL2 MARS LCS DERIVED ATMOSPHERIC OPTICAL DEPTH V1.0      
#   Description:  Viking Lander camera images of the Sun were used to compute total normal atmospheric
#                 optical depth at the two landing sites over a period of about 900 Mars days. 
#   Source:       PDS - Planetary Data System
#   URL:          https://pds.nasa.gov/ds-view/pds/viewProfile.jsp?dsid=VL1/VL2-M-LCS-5-ATMOS-OPTICAL-DEPTH-V1.0
#
# Mars Exploration Rovers (Spirit and Opportunity)
#   Dataset:      Pancam Atmospheric Opacity Science RDRs
#   Description:  Mars Exploration Rover Pancam Atmospheric Opacity for both the MER-1 and MER-2 rovers.
#   Source:       PDS - Geosciences Node
#   URL:          http://pds-geosciences.wustl.edu/missions/mer/geo_mer_datasets.htm
#
# Mars Science Laboratory (Curiosity)
#   Dataset:  
#   Description:
#   Source:   
#   URL:
#
# InSight
#   Dataset:
#   Description:
#   Source:   
#   URL:

# Citation Guideline:
# https://pds.nasa.gov/datastandards/pds3/citing-pds3-data.shtml

#library(pds3)
library(here)
library(wesanderson)

# Plot function.
tau_eq = dget(here("functions", "tau.R"))

Ls_seq = seq(0, 360, 1) # [deg]
phi_oppy = -2.28
phi_spirit = -14.6
model = 1 


# For some reason not working when doing tau_eq(phi, Ls_seq, 1).
# So get taus iteratively.
tau_seq = c()
for(Ls in Ls_seq){
  tau = tau_eq(phi_oppy, Ls, model)
  tau_seq = c(tau_seq, tau)
}

dev.new()
plot(Ls_seq, tau_seq,
     xlab="Areocentric Longitude, Ls [deg]",
     ylab="Optical Depth, τ",
     ylim=c(0, 7),
     type="l",
     lty=2,
     main="Optical Depth as a Function of Areocentric Longitude",
     col="red")

data_base_url = "http://pds-geosciences.wustl.edu/mer/mer1_mer2-m-pancam-5-atmos-opacity-v1/merao_1xxx/data/"

# Opportunity, aka MER-B (Mars Exploration Rover – B) or MER-1.
oppy_lbl_1 = paste(data_base_url, "1tau440_5106_20181207a.lbl", sep="")
oppy_tab_1 = paste(data_base_url, "1tau440_5106_20181207a.tab", sep="")

oppy_lbl_2 = paste(data_base_url, "1tau880_5106_20181207a.lbl", sep="")
oppy_tab_2 = paste(data_base_url, "1tau880_5106_20181207a.tab", sep="")

# Spirit, aka MER-A (Mars Exploration Rover – A) or MER-2.
spirit_lbl_1 = paste(data_base_url, "2tau440_2209_20110214a.lbl", sep="")
spirit_tab_1 = paste(data_base_url, "2tau440_2209_20110214a.tab", sep="")

spirit_lbl_2 = paste(data_base_url, "2tau880_2209_20110214a.lbl", sep="")
spirit_tab_2 = paste(data_base_url, "2tau880_2209_20110214a.tab", sep="")

lbl = oppy_lbl_2 
tab = oppy_tab_2

# req = curl::curl_fetch_memory(lbl)
# dat = rawToChar(req$content)
# res = pds3_read(dat)
# print(res$odl$DATA_SET_ID)


conn = file(tab, open="r")
lines = readLines(conn)
close(conn)

# Clean data.
# Remove header.
data_char = lines[seq(10, length(lines), 1)]
# Remove whitespace and quotation marks
data_char = gsub('\\s|\"', "", data_char, perl=TRUE)

data = read.csv(text=data_char, header=FALSE)

Ls_vect = unname(unlist(data[2]))
Sols = unname(unlist(data[4]))
taus = unname(unlist(data[7]))

Ls_yr = c()
Sols_yr = c()
taus_yr = c()

Ls_prev = 999
year_count = 0
index = 1
for(Ls in Ls_vect){
  # Handle negative values of Ls.
  Ls = ifelse(Ls<0, Ls+360, Ls)
  
  # New year, Gone from Ls=360-ish to Ls=0-ish.
  if(Ls < Ls_prev){
    
    if(!is.null(Ls_yr)){
      lines(Ls_yr, taus_yr, type = "l",
            col=wes_palette("FantasticFox1", 10, type="continuous")[year_count+1])
    }
    Ls_yr = c(Ls)
    Sols_yr = c(Sols[index])
    taus_yr = c(taus[index])
    
    year_count = year_count + 1
    
  }else{
    # Still in the same year.
    Ls_yr = c(Ls_yr, Ls)
    Sols_yr = c(Sols_yr, c(Sols[index]))
    taus_yr = c(taus_yr, c(taus[index]))
  }
  
  Ls_prev = Ls
  index = index + 1
}
