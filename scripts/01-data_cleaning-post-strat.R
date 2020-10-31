#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Brian Diep
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Download the 2018 ACS dataset as detailed in the readme and save it to 
#   inputs/data
# - Don't share that data


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00003.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

census_data <- 
  raw_data %>% 
  dplyr::select(
         stateicp,
         sex, 
         age, 
         race, 
         educd,
         hispan,
         labforce,
         inctot)
rm(raw_data)
         

#### Further Cleaning ####

# Make age into groups
census_data <- 
  census_data %>%
  mutate(age = case_when(
    between(age, 0, 24) ~ "0-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    between(age, 75, Inf) ~ "75+"))

# Change state column to match requirements
census_data <-
  census_data %>%
  rename(state = stateicp)

# Clean income
census_data <-
  census_data %>%
  mutate(income = case_when(
    between(inctot, -Inf, 20000) ~ "Under Poverty Line",
    between(inctot, 20001, 44999) ~ "Low Income",
    between(inctot, 45000, 99999) ~ "Lower Middle Income",
    between(inctot, 100000, 149999) ~ "Upper Middle Income",
    between(inctot, 150000, 199999) ~ "High Income",
    between(inctot, 200000, Inf) ~ "Eat the rich"
  ))

# Clean race
census_data <-
  census_data %>%
  mutate(race = case_when(
    race == "black/african american/negro" ~ "black",
    race == "chinese" | race == "japanese" | race == "other asian or pacific islander" ~ "asian or pacific islander",
    race == "other race, nec" ~ "other",
    race == "two major races" | race == "three or more major races" ~ "mixed",
    race == "white" ~ "white",
    race == "american indian or alaska native" ~ "native"
  ))

# Clean hispanic
census_data <-
  census_data %>%
  mutate(hispan, ifelse(hispan == "not hispanic", "not hispanic", "hispanic"))
         