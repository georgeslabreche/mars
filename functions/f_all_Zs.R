library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

function(al=0.1, pub_year=1990){
  as.numeric(gsub("X", "", colnames(f_build_df(al, pub_year))))
}


