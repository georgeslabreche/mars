library(here)
library(devtools)

# Read CSV files and make binary data files.
df_netflux_0p1_lookup_v1 = read.csv(here("data-raw/csv", "netflux_al_0-1_table_iii_1990.csv"))
use_data(df_netflux_0p1_lookup_v1, overwrite=TRUE)

df_netflux_0p1_lookup_v2 = read.csv(here("data-raw/csv", "netflux_al_0-1_table_iii_1991_update.csv"))
use_data(df_netflux_0p1_lookup_v2, overwrite=TRUE)

df_netflux_0p4_lookup_v2 = read.csv(here("data-raw/csv", "netflux_al_0-4_table_iii_1991_update.csv"))
use_data(df_netflux_0p4_lookup_v2, overwrite=TRUE)

df_netflux_k0_coeffs = read.csv(here("data-raw/csv", "netflux_k0_coefficients_1990_update.csv"))
df_netflux_k0_coeffs = df_netflux_k0_coeffs[-c(1)]
use_data(df_netflux_k0_coeffs, overwrite=TRUE)

df_netflux_k1_coeffs = read.csv(here("data-raw/csv", "netflux_k1_coefficients_1990_update.csv"))
df_netflux_k1_coeffs = df_netflux_k1_coeffs[-c(1)]
use_data(df_netflux_k1_coeffs, overwrite=TRUE)