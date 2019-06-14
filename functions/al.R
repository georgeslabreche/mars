# The albedo function.
# Uses the looktup Table I published in "Solar radiation on Mars: Update 1990."
#
# Based on following publication:
#   Appelbaum, Joseph & Landis, Geoffrey & Sherman, I. (1991). Solar radiation on Mars—Update 1991. Solar Energy. 50. 35-51. 10.1016/0038-092X(93)90006-A:
#   https://www.researchgate.net/publication/223850868_Solar_radiation_on_Mars-Update_1991

al = read.csv(here("data/normalized_net_flux_function/", "albedo_1991_update.csv"))
rownames(al) = al[,1]
al = al[-c(1)]

# The albedo function.
#   longitude       - From -180 to 180 [deg].
#   latitude (phi)  - From -90 to 90 [deg].
function(longitude, latitude){
  return(al[toString(longitude), paste("X", gsub("-", ".", latitude), sep="")])
}
