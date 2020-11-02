#### Preamble ####
# Purpose: Running Bayesian model and saving the results for analysis 
#         (Download from instructions given in readme file)
# Author: John Cao, Brian Diep, Jonathan Tillman, Tanya Woloshansky
# Data: 02 November 2020
# Contact: brian.diep@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have run both other scripts and have both datasets ready
# - Don't upload that data file!
# - Please run this before you knit the main RMarkdown file, doing the
#   post stratification takes a lot of time

library(tidyverse)
library(broom)
library(brms)
library(here)

# Fit Bayesian model
model_states <- brm(vote_2020 ~ gender + age + household_income +
                      race_ethnicity + hispanic + employment,
                    data = survey_data,
                    family = bernoulli(),
                    file = "outputs/model/brms_model_states8",
                    chains=6
)

model <- read_rds("outputs/model/brms_model_states8.rds")

# Increase memory allocated to R
memory.limit(size=80000)

pop_vote <- plyr::count(census_data, c("state", "gender", "age",
                                       "race_ethnicity", "hispanic",
                                       "household_income", "employment"))

# Do post stratification
pop_vote <- pop_vote %>%
  group_by(state) %>%
  mutate(prop = freq / sum(freq)) %>%
  ungroup()

post_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(state, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(state) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

# Winner takes all, convert all > 0.5 Biden vote predictions into binary
# response
post_stratified_estimates <- post_stratified_estimates %>%
  mutate(winner = ifelse(mean > 0.5, 1, 0))

write_csv(post_stratified_estimates, "post_stratified_data.csv")