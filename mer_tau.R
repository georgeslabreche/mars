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
# Phoenix
#   Dataset:      PHOENIX Atmospheric Opacity data.
#   Desctiption:  Derived data from the SSI instrument measuring the atmospheric opacity for sols 1-151.
#   Source:       PDS - Planetary Atmospheres Node
#   URL:          https://pds-atmospheres.nmsu.edu/data_and_services/atmospheres_data/Mars/Mars.html
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
phi_vl1 = 22.27
phi_vl2 = 47.64
phi_phoenix = 68.22
model = 1

# For some reason not working when doing tau_eq(phi, Ls_seq, 1).
# So get taus iteratively.
tau_seq = c()
for(Ls in Ls_seq){
  tau = tau_eq(phi_vl1, Ls, model)
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


tau_lines = function(Ls_vect, Sols, taus, sol_start, sol_end, include_years){
  Ls_yr = c()
  Sols_yr = c()
  taus_yr = c()
  
  Ls_prev = 999
  year_count = 0
  index = 1
  
  for(sol in Sols){
    if(sol >= sol_start && sol <= sol_end){
      # Handle negative values of Ls.
      Ls = ifelse(c(Ls_vect[index])<0, c(Ls_vect[index])+360, c(Ls_vect[index]))
      
      # New year, Gone from Ls=360-ish to Ls=0-ish.
      if(Ls < Ls_prev){
        
        if(!is.null(Ls_yr)){
          if(is.null(include_years) || year_count %in% include_years){
            lines(Ls_yr, taus_yr, type = "l",
                  col=wes_palette("FantasticFox1", 10, type="continuous")[year_count+1])
          }
        }
        
        Ls_yr = c(Ls)
        Sols_yr = c(Sols[index])
        taus_yr = c(taus[index])
        
        year_count = year_count + 1
        
      }else{
        # Still in the same year.
        Ls_yr = c(Ls_yr, Ls)
        Sols_yr = c(Sols_yr, Sols[index])
        taus_yr = c(taus_yr, taus[index])
      }
      
      Ls_prev = Ls
      index = index + 1
    }
  }
}

mer_tau_lines = function(rover_id="MER-A", sol_start=0, sol_end=5106, include_years=NULL) {
  mer_data_url = "http://pds-geosciences.wustl.edu/mer/mer1_mer2-m-pancam-5-atmos-opacity-v1/merao_1xxx/data/"
  
  # Opportunity, aka MER-B (Mars Exploration Rover – B) or MER-1.
  oppy_lbl_1 = paste(mer_data_url, "1tau440_5106_20181207a.lbl", sep="")
  oppy_tab_1 = paste(mer_data_url, "1tau440_5106_20181207a.tab", sep="")
  
  oppy_lbl_2 = paste(mer_data_url, "1tau880_5106_20181207a.lbl", sep="")
  oppy_tab_2 = paste(mer_data_url, "1tau880_5106_20181207a.tab", sep="")
  
  # Spirit, aka MER-A (Mars Exploration Rover – A) or MER-2.
  spirit_lbl_1 = paste(mer_data_url, "2tau440_2209_20110214a.lbl", sep="")
  spirit_tab_1 = paste(mer_data_url, "2tau440_2209_20110214a.tab", sep="")
  
  spirit_lbl_2 = paste(mer_data_url, "2tau880_2209_20110214a.lbl", sep="")
  spirit_tab_2 = paste(mer_data_url, "2tau880_2209_20110214a.tab", sep="")
  
  data_url = NULL
  if(toupper(rover_id) %in% c("MER-A", "MERA", "MER-2", "MER2", "SPIRIT")){
    data_url = spirit_tab_2
    
  }else if(toupper(rover_id) %in% c("MER-B", "MERB", "MER-1", "MER1", "OPPORTUNITY", "OPPY")){
    data_url = oppy_tab_2
    
  }else{
    stop("Invalid input for rover_id, should either be 'MER-A' for Spirit or 'MER-B' for Opportunity.")
  }
  
  conn = file(data_url, open="r")
  lines = readLines(conn)
  close(conn)
  
  # Clean the data so that it is compliant with CSV format.
  # Remove header text.
  data_char = lines[seq(10, length(lines), 1)]
  
  # Remove whitespace and quotation marks.
  data_char = gsub('\\s|\"', "", data_char, perl=TRUE)
  
  # Read as CSV.
  data = unname(read.csv(text=data_char, header=FALSE))
  
  Ls_vect = unlist(data[2])
  Sols = unlist(data[4])
  taus = unlist(data[7])
  
  tau_lines(Ls_vect, Sols, taus, sol_start, sol_end, include_years)
}

vl_tau_lines = function(lander_id="VL1", sol_start=0, sol_end=1147, include_years=NULL) {
  vl_data_url = "https://atmos.nmsu.edu/pdsd/archive/data/vl1vl2-m-lcs-5-atmos-optical-depth-v10/vl_1001/data/vl_opac.dat"
  
  if(!(toupper(lander_id) %in% c("VL-1", "VL1", "VL-2", "VL2"))){
    stop("Invalid input for rover_id, should either be 'MER-A' for Spirit or 'MER-B' for Opportunity.")
  }
    
  conn = file(vl_data_url, open="r")
  data_char = readLines(conn)
  close(conn)
  
  # Clean the data so that it is compliant with CSV format.
  # Put commas where there are whitespaces in between values.
  data_char = substring(gsub("\\s+", ",", data_char, perl=TRUE), 2)
  
  # Read as CSV.
  data = read.csv(text=data_char, header=FALSE)
  
  # Only grab data for the lander we are interested in.
  data = subset(data, V1==gsub("-", "", lander_id))
  
  Ls_vect = unlist(data[10])
  Sols = unlist(data[4])
  taus = unlist(data[7])
  
  tau_lines(Ls_vect, Sols, taus, sol_start, sol_end, include_years)
}

phoenix_tau_lines = function(lander_id="VL1", sol_start=0, sol_end=1147, include_years=NULL) {
}

####################################################
# Mars Exploration Rovers (Spirit and Opportunity) #
####################################################
mer_tau_lines(MER-A)

##################
# Viking Landers #
##################
#vl_tau_lines()


