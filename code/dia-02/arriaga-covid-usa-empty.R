# Setup -------------------------------------------------------------------
# Run this script from the repository root.
load("data/dia-02/arriaga-covid-usa-data.RData")

library(tidyverse)
library(data.table)

# Compare life expectancies  ----------------------------------------------

data %>% filter(year==2020,age==0,sex==0) %>% pull(ex) %>% unique() - 
  data %>% filter(year==2019,age==0,sex==0) %>% pull(ex) %>% unique()

# Decomposition -----------------------------------------------------------

# Age

data_nocause <- data %>% 
  select(!c(cause,prop)) %>% 
  distinct
  
  
