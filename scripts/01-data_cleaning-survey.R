#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from the Nationscape
#         Dataset (Download from instructions given in readme file)
# Author: John Cao, Brian Diep, Jonathan Tillman, Tanya Woloshansky
# Data: 02 November 2020
# Contact: brian.diep@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from the Nationscape Dataset and save
# the folder that you're interested in to inputs/data 
# - Don't upload that data file!


#### Workspace setup ####
library(haven)
library(tidyverse)

# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/data/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
survey_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         age)

rm(raw_data)

# Filter out third-party and non-voters

survey_data <-
  survey_data %>%
  filter(vote_intention %in% c("Yes, I will vote", "Not sure", 
                               "No, I will not vote but I am eligible"))

survey_data <-
  survey_data %>% 
  filter(vote_2020 %in% c('Joe Biden', 'Donald Trump'))

# 1 is a vote for Biden, 0 is a vote for Trump
survey_data <-
  survey_data %>%
  mutate(vote_2020 = ifelse(vote_2020 == 'Joe Biden', 1, 0))

# Make some age-groups
survey_data <- 
  survey_data %>%
  mutate(age = case_when(
    between(age, 0, 24) ~ "18-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    between(age, 75, Inf) ~ "75+"))

# Make sex into binary variable
survey_data <-
  survey_data %>%
  mutate(gender = ifelse(gender == "Male", "Male", "Female"))

# Clean income
#Filter out null values

survey_data <-
  survey_data %>% filter(!is.na(household_income))

survey_data <-
  survey_data %>%
  mutate(household_income = case_when(
    household_income %in% c("Less than $14,999", "$15,000 to $19,999") ~ "Under Poverty Line",
    household_income %in% c("$20,000 to $24,999", "$25,000 to $29,999",
                            "$30,000 to $34,999", "$35,000 to $39,999",
                            "$40,000 to $44,999") ~ "Low Income",
    household_income %in% c("$45,000 to $49,999", "$50,000 to $54,999",
                            "$55,000 to $59,999", "$60,000 to $64,999",
                            "$65,000 to $69,999", "$70,000 to $74,999",
                            "$75,000 to $79,999", "$80,000 to $84,999",
                            "$85,000 to $89,999", "$90,000 to $94,999",
                            "$95,000 to $99,999") ~ "Lower Middle Income",
    household_income %in% c("$100,000 to $124,999",
                            "$125,000 to $149,999") ~ "Upper Middle Income",
    household_income %in% c("$150,000 to $174,999",
                            "$175,000 to $199,999") ~ "High Income",
    household_income %in% c("$200,000 to $249,999", "$250,000 and above") ~ "Eat the rich"
    ))

# Make hispanic binary variable
survey_data <-
  survey_data %>%
  mutate(hispanic = ifelse(hispanic == "Not Hispanic", "Not Hispanic", "Hispanic"))

#Group up race data
survey_data <-
  survey_data %>%
  mutate(race_ethnicity = case_when(
    race_ethnicity == "Black, or African American" ~ "Black",
    race_ethnicity %in% c("Asian (Japanese)", "Asian (Japanese)", 
                          "Asian (Other)", "Asian (Chinese)", "Asian (Korean)",
                          "Asian (Filipino)", "Asian (Vietnamese)", 
                          "Asian (Asian Indian)", "Pacific Islander (Samoan)", 
                          "Pacific Islander (Native Hawaiian)",
                          "Pacific Islander (Other)",
                          "Pacific Islander (Guamanian)"
                          ) ~ "Asian/Pacific Islander",
    race_ethnicity == "Some other race" | is.na(race_ethnicity)~ "Other/mixed",
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "American Indian or Alaska Native" ~ "Native American"
  ))

# Group up employment

survey_data <- survey_data %>%
  filter(!is.na(employment))

survey_data <- survey_data %>%
  filter(employment %ni% c("Other:"))

survey_data <-
  survey_data %>%
  mutate(employment = case_when(
    employment %in% c("Full-time employed", "Part-time employed", "Self-employed") ~ "Employed",
    employment %in% c("Homemaker", "Retired", "Permanently disabled", "Student") ~ "Not in labour force",
    employment %in% c("Unemployed or temporarily on layoff") ~ "Unemployed"
  ))

write_csv(survey_data, "survey_data.csv")