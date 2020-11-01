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
library(fastmatch)

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

#census_data <- 
#  census_data %>% 
#  mutate(age_rank = fmatch(census_data$age, c("18-24", "25-34", "35-44", "45-54",
#                                              "55-64", "65-74", "75+")))

# Change state column to match requirements
names(census_data)[names(census_data) == "stateicp"] <- "state"

#census_data <-
#  census_data %>%
#  mutate(state = fmatch(census_data$state, c("alaska","alabama","arkansas","arizona",
#                                             "california","colorado","connecticut",
#                                             "district of columbia","delaware","florida",
#                                             "georgia","hawaii","iowa","idaho",
#                                             "illinois","indiana","kansas",
#                                             "kentucky","louisiana","massachusetts",
##                                             "maryland","maine","michigan","minnesota",
#                                             "missouri","mississippi","montana",
#                                             "north carolina","north dakota",
#                                             "nebraska","new hampshire","new jersey",
#                                             "new mexico","nevada","new york",
#                                             "ohio","oklahoma","oregon","pennsylvania",
#                                             "rhode island","south carolina",
#                                             "south dakota","tennessee","texas",
#                                             "utah","virginia","vermont","washington",
#                                             "wisconsin","west virginia","wyoming")))


census_data <-
  census_data %>%
  mutate(state = case_when(
    state == "alaska" ~ "AK",
    state == "alabama" ~ "AL",
    state == "arkansas" ~ "AR",
    state == "arizona" ~ "AZ",
    state == "california" ~ "CA",
    state == "colorado" ~ "CO",
    state == "connecticut" ~ "CT",
    state == "district of columbia" ~ "DC",
    state == "delaware" ~ "DE",
    state == "florida" ~ "FL",
    state == "georgia" ~ "GA",
    state == "hawaii" ~ "HI",
    state == "iowa" ~ "IA",
    state == "idaho" ~ "ID",
    state == "illinois" ~ "IL",
    state == "indiana" ~ "IN",
    state == "kansas" ~ "KS",
    state == "kentucky" ~ "KY",
    state == "louisiana" ~ "LA",
    state == "massachusetts" ~ "MA",
    state == "maryland" ~ "MD",
    state == "maine" ~ "ME",
    state == "michigan" ~ "MI",
    state == "minnesota" ~ "MN",
    state == "missouri" ~ "MO",
    state == "mississippi" ~ "MS",
    state == "montana" ~ "MT",
    state == "north carolina" ~ "NC",
    state == "north dakota" ~ "ND",
    state == "nebraska" ~ "NE",
    state == "new hampshire" ~ "NH",
    state == "new jersey" ~ "NJ",
    state == "new mexico" ~ "NM",
    state == "nevada" ~ "NV",
    state == "new york" ~ "NY",
    state == "ohio" ~ "OH",
    state == "oklahoma" ~ "OK",
    state == "oregon" ~ "OR",
    state == "pennsylvania" ~ "PA",
    state == "rhode island" ~ "RI",
    state == "south carolina" ~ "SC",
    state == "south dakota" ~ "SD",
    state == "tennessee" ~ "TN",
    state == "texas" ~ "TX",
    state == "utah" ~ "UT",
    state == "virginia" ~ "VA",
    state == "vermont" ~ "VT",
    state == "washington" ~ "WA",
    state == "wisconsin" ~ "WI",
    state == "west virginia" ~ "WV",
    state == "wyoming" ~ "WY"
  ))

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

names(census_data)[names(census_data) == "ftotinc"] <- "household_income"
#census_data <- 
#  census_data %>% 
#  mutate(income_rank = fmatch(census_data$ftotinc, c("Under Poverty Line", "Low Income",
#                                                              "Lower Middle Income", "Upper Middle Income",
#                                                              "High Income", "Eat the rich")))

# Clean race/ethnicity data

#Group up races
census_data <-
  census_data %>%
  mutate(race = case_when(
    race == "black/african american/negro" ~ "Black",
    race == "chinese" | race == "japanese" | race == "other asian or pacific islander" ~ "Asian/Pacific Islander",
    race %in% c("other race, nec", "two major races", "three or more major races") ~ "Other/mixed",
    race == "white" ~ "White",
    race == "american indian or alaska native" ~ "Native American"
  ))

names(census_data)[names(census_data) == "race"] <- "race_ethnicity"

# Make hispanic binary variable
census_data <-
  census_data %>%
  mutate(hispan = ifelse(hispan == "not hispanic", "Not Hispanic", "Hispanic"))

names(census_data)[names(census_data) == "hispan"] <- "hispanic"

# Group up employment
# 1 = Employed
# 2 = Out of labour market
# 3 = Unemployed
# 4 = Other

census_data <-
  census_data %>%
  filter(empstat %ni% c("n/a"))

census_data <-
  census_data %>%
  mutate(empstat = case_when(
    empstat == "employed" ~ "Employed",
    empstat == "not in labor force" ~ "Not in labour force",
    empstat == "unemployed" ~ "Unemployed"
  ))

names(census_data)[names(census_data) == "empstat"] <- "employment"

# Make sex into gender
names(census_data)[names(census_data) == "sex"] <- "gender"

# Make sex binary
# 1 = Male
# 0 = Female
census_data <-
  census_data %>%
  mutate(gender = ifelse(gender == "male", "Male", "Female"))


## Get proportions (popular vote)
#vote <- vote %>%
#  group_by(state) %>%
#  mutate(prop = freq / sum(freq)) %>%
#  ungroup()