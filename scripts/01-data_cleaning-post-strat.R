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
library(plyr)

# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00005.dta")

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
         hispan,
         empstat,
         ftotinc)
rm(raw_data)
         
#educd
#### Further Cleaning ####

'%ni%' <- Negate('%in%')

# Clean Age
# Remove non-voting aged
census_data <- 
  census_data %>%
  filter(age %ni% c("less than 1 year old", "2", "3", "4", "5", "6",
                  "7", "8", "9", "10", "11", "12", "13", "14", "15",
                  "16", "17"))

# Make age into groups
census_data <- 
  census_data %>%
  mutate(age = case_when(
    between(age, 0, 24) ~ "18-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    between(age, 75, Inf) ~ "75+"))

# Change state column to match requirements
names(census_data)[names(census_data) == "stateicp"] <- "state"
#census_data <-
#  census_data %>%
#  rename(state = stateicp)

# Clean income
# Remove 999999 responses (N/A code)

census_data <-
  census_data %>%
  filter(ftotinc != 9999999)


census_data <-
  census_data %>%
  mutate(ftotinc = case_when(
    between(ftotinc, -Inf, 19999) ~ "Under Poverty Line",
    between(ftotinc, 20000, 44999) ~ "Low Income",
    between(ftotinc, 45000, 99999) ~ "Lower Middle Income",
    between(ftotinc, 100000, 149999) ~ "Upper Middle Income",
    between(ftotinc, 150000, 199999) ~ "High Income",
    between(ftotinc, 200000, Inf) ~ "Eat the rich"
  ))

# Clean race/ethnicity data

#Group up races
census_data <-
  census_data %>%
  mutate(race = case_when(
    race == "black/african american/negro" ~ "black",
    race == "chinese" | race == "japanese" | race == "other asian or pacific islander" ~ "asian or pacific islander",
    race %in% c("other race, nec", "two major races", "three or more major races") ~ "other/mixed",
    race == "white" ~ "white",
    race == "american indian or alaska native" ~ "native"
  ))

# Make hispanic binary variable
census_data <-
  census_data %>%
  mutate(hispan = ifelse(hispan == "not hispanic", "not hispanic", "hispanic"))

# Make education
#census_data <-
#  census_data %>%
#  mutate(education = case_when(
#    educd %in% c("no schooling completed", "nursery school, preschool", "kindergarten",
#                 "grade 1", "grade 2", "grade 3", "grade 4", "grade 5", "grade 6",
#                 "grade 7", "grade 8", "grade 9", "grade 10", "grade 11", 
#                 "grade 12, no diploma") ~ "Did not complete high school",
#    educd %in% c("regular high school diploma", "ged or alternative credential") ~ "High school or equivalent",
#    educd %in% c("some college, but less than 1 year", "1 or more years of college credit, no degree",
#                 "associate's degree, type not specified", "bachelor's degree", )
    
    
#  ))

## Get proportions (popular vote)
d <- count(census_data, c("state", "sex", "age", "race", "hispan", "ftotinc", "empstat"))
d <- d %>%
  group_by(state) %>%
  mutate(prop = freq / sum(freq)) %>%
  ungroup()