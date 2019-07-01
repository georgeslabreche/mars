# Runs all the unit tests.

library(here)
library(testthat) 

test_results = test_dir(here("test"),reporter="minimal")