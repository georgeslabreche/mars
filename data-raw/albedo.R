library(here)
library(devtools)

# Read albedo CSV file.
df_albedo = read.csv(here("data-raw/csv", "albedo_1991_update.csv"))

# Rename column labels (beta/phi). Use 'm' for minus/negative and 'p' for plus/positive.
colnames(df_albedo) = gsub("X\\.", "m", colnames(df_albedo))
colnames(df_albedo) = gsub("X", "p", colnames(df_albedo))
colnames(df_albedo) = gsub("p0", "0", colnames(df_albedo))

# Make binary data.
use_data(df_albedo, overwrite=TRUE)