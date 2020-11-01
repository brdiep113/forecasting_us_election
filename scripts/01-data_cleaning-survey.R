#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(fastmatch)

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


#### What else???? ####

# Filter out third-party and non-voters
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

survey_data <- 
  survey_data %>% 
  mutate(age_rank = fmatch(survey_data$age, c("18-24", "25-34", "35-44", "45-54",
                                              "55-64", "65-74", "75+")))

# Make sex into binary variable
survey_data <-
  survey_data %>%
  mutate(gender = ifelse(gender == "Male", 1, 0))

# Make income into category

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

survey_data <- 
  survey_data %>% 
  mutate(income_rank = fmatch(survey_data$household_income, c("Under Poverty Line", "Low Income",
                                                 "Lower Middle Income", "Upper Middle Income",
                                                 "High Income", "Eat the rich")))

# Make hispanic binary variable
# 1 = Hispanic, 0 = Not Hispanic
survey_data <-
  survey_data %>%
  mutate(hispanic = ifelse(hispanic == "Not Hispanic", 0, 1))

#Group up races
# 1 = Black
# 2 = Asian/Pacific Islander
# 3 = Other/Mixed
# 4 = White
# 5 = Native
survey_data <-
  survey_data %>%
  mutate(race_ethnicity = case_when(
    race_ethnicity == "Black, or African American" ~ 1,
    race_ethnicity %in% c("Asian (Japanese)", "Asian (Japanese)", 
                          "Asian (Other)", "Asian (Chinese)", "Asian (Korean)",
                          "Asian (Filipino)", "Asian (Vietnamese)", 
                          "Pacific Islander (Samoan)", 
                          "Pacific Islander (Native Hawaiian)",
                          "Pacific Islander (Other)",
                          "Pacific Islander (Guamanian)"
                          ) ~ 2,
    race_ethnicity == "Some other race" ~ 3,
    race_ethnicity == "White" ~ 4,
    race_ethnicity == "American Indian or Alaska Native" ~ 5
  ))

# Group up employment
# 1 = Employed
# 2 = Out of labour market
# 3 = Unemployed
# 4 = Other

survey_data <-
  survey_data %>%
  mutate(employment = case_when(
    employment %in% c("Full-time employed", "Part-time employed", "Self-employed") ~ 1,
    employment %in% c("Homemaker", "Retired", "Permanently disabled", "Student") ~ 2,
    employment %in% c("Unemployed or temporarily on layoff") ~ 3,
    employment %in% c("Other") | is.na(employment) ~ 4
  ))

survey_data <-
  survey_data %>%
  mutate(state = fmatch(survey_data$state, c("AK","AL","AR","AZ","CA","CO","CT",
                                             "DC","DE","FL","GA","HI","IA","ID",
                                             "IL","IN","KS","KY","LA","MA","MD",
                                             "ME","MI","MN","MO","MS","MT","NC",
                                             "ND","NE","NH","NJ","NM","NV","NY",
                                             "OH","OK","OR","PA","RI","SC","SD",
                                             "TN","TX","UT","VA","VT","WA","WI",
                                             "WV","WY")))
  
# Change state column to match requirements
#names(census_data)[names(census_data) == "stateicp"] <- "state"

# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

mylogit <- glm(vote_2020 ~ as.factor(state) + gender + 
               as.factor(race_ethnicity) + as.factor(income_rank) +
                 hispanic + as.factor(employment) + as.factor(age_rank),
               data=survey_data, family="binomial")

summary(mylogit)