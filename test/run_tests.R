# Runs all the unit tests.

library(here)
library(testthat) 

test_file(here("test", "test_H_obh.R"), reporter="minimal")

print("I_obh:")
test_file(here("test", "test_I_obh.R"), reporter="minimal")

print("I_h:")
test_file(here("test", "test_I_h.R"), reporter="minimal")

print("I_bh:")
test_file(here("test", "test_I_bh.R"), reporter="minimal")

print("I_dh:")
test_file(here("test", "test_I_dh.R"), reporter="minimal")

print("H_obh:")
test_file(here("test", "test_H_obh.R"), reporter="minimal")

print("H_h:")
test_file(here("test", "test_H_h.R"), reporter="minimal")

print("H_bh:")
test_file(here("test", "test_H_bh.R"), reporter="minimal")

print("H_dh:")
test_file(here("test", "test_H_dh.R"), reporter="minimal")

