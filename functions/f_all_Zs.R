library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

function(){
  as.numeric(gsub("X", "", colnames(f_build_df())))
}


