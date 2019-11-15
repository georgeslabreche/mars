library(here)
library(devtools)

# Read CSV files and make binary data files.
df_netflux_0p1_1990 = read.csv(here("data-raw/csv", "netflux_al_0-1_table_iii_1990.csv"))
use_data(df_netflux_0p1_1990, overwrite=TRUE)

df_netflux_0p1_1991 = read.csv(here("data-raw/csv", "netflux_al_0-1_table_iii_1991_update.csv"))
use_data(df_netflux_0p1_1991, overwrite=TRUE)

df_netflux_0p4_1991 = read.csv(here("data-raw/csv", "netflux_al_0-4_table_iii_1991_update.csv"))
use_data(df_netflux_0p4_1991, overwrite=TRUE)

df_netflux_k0_1990 = read.csv(here("data-raw/csv", "netflux_k0_coefficients_1990_update.csv"))
df_netflux_k0_1990 = df_netflux_k0_1990[-c(1)]
use_data(df_netflux_k0_1990, overwrite=TRUE)

df_netflux_k1_1990 = read.csv(here("data-raw/csv", "netflux_k1_coefficients_1990_update.csv"))
df_netflux_k1_1990 = df_netflux_k1_1990[-c(1)]
use_data(df_netflux_k1_1990, overwrite=TRUE)