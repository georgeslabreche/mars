# Runs all the unit tests.

library(here)
library(testthat) 

reporters = list(
  minimal="minimal",
  summary="summary",
  progress="progress"
)

reporter = reporters$minimal

print("Sunrise:")
# FIXME: This test fails for high values of beta angles >= 40 deg.
#test_file(here("test", "test_sunrise.R"), reporter=reporter)

# print("Declination (delta):")
# test_file(here("test", "test_declination.R"), reporter=reporter)
# 
# print("H_obh:")
# test_file(here("test", "test_H_obh.R"), reporter=reporter)
# 
# print("I_obh:")
# test_file(here("test", "test_I_obh.R"), reporter=reporter)
# 
# print("I_h:")
# test_file(here("test", "test_I_h.R"), reporter=reporter)
# 
# print("I_bh:")
# test_file(here("test", "test_I_bh.R"), reporter=reporter)
# 
# print("I_dh:")
# test_file(here("test", "test_I_dh.R"), reporter=reporter)
# 
# print("H_obh:")
# test_file(here("test", "test_H_obh.R"), reporter=reporter)
# 
# print("H_h:")
# test_file(here("test", "test_H_h.R"), reporter=reporter)
# 
# print("H_bh:")
# test_file(here("test", "test_H_bh.R"), reporter=reporter)
# 
# print("H_dh:")
# test_file(here("test", "test_H_dh.R"), reporter=reporter)
# 
print("H_h_beta:")
test_file(here("test", "test_H_h_beta.R"), reporter=reporter)