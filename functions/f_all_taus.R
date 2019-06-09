library(here)

f_build_df = dget(here("functions", "f_build_df.R"))

function(){
  as.numeric(rownames(f_build_df()))
}