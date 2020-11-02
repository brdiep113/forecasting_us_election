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

# Do post stratification (state)
pop_vote <- pop_vote %>%
  group_by(state) %>%
  mutate(prop = freq / sum(freq)) %>%
  ungroup()

state_post_stratified_estimates <- model %>%
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
state_post_stratified_estimates <- state_post_stratified_estimates %>%
  mutate(winner = ifelse(mean > 0.5, 1, 0))

write_csv(state_post_stratified_estimates, "outputs/post_stratified_csv/state_post_stratified_data.csv")

pop_vote$prop <- NULL

# (gender)
pop_vote <- pop_vote %>%
  group_by(gender) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

gender_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(gender, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(gender) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(gender_stratified_estimates, "outputs/post_stratified_csv/gender_post_stratified_data.csv")

pop_vote$prop <- NULL

# (race/ethnicity)
pop_vote <- pop_vote %>%
  group_by(race_ethnicity) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

race_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(race_ethnicity, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(race_ethnicity) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(race_stratified_estimates, "outputs/post_stratified_csv/race_post_stratified_data.csv")

pop_vote$prop <- NULL

# (hispanic)
pop_vote <- pop_vote %>%
  group_by(hispanic) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

hispanic_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(hispanic, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(hispanic) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(hispanic_stratified_estimates, "outputs/post_stratified_csv/hispanic_post_stratified_data.csv")

pop_vote$prop <- NULL

# (income)
pop_vote <- pop_vote %>%
  group_by(household_income) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

income_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(household_income, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(household_income) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(income_stratified_estimates, "outputs/post_stratified_csv/income_post_stratified_data.csv")

# (employment_status)
pop_vote <- pop_vote %>%
  group_by(employment) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

employment_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(employment, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(employment) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(employment_stratified_estimates, "outputs/post_stratified_csv/employment_post_stratified_data.csv")

# (employment_status)
pop_vote <- pop_vote %>%
  group_by(employment) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

employment_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(employment, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(employment) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(employment_stratified_estimates, "outputs/post_stratified_csv/employment_post_stratified_data.csv")

# (age)
pop_vote <- pop_vote %>%
  group_by(age) %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

age_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(age, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(age) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(age_stratified_estimates, "outputs/post_stratified_csv/age_post_stratified_data.csv")

# (everything)
pop_vote <- pop_vote %>%
  mutate(prop = freq/ sum(freq)) %>%
  ungroup()

fullpop_stratified_estimates <- model %>%
  tidybayes::add_predicted_draws(newdata=pop_vote) %>%
  rename(biden_predict =.prediction) %>%
  mutate(biden_predict_prop =biden_predict*prop) %>%
  group_by(state, .draw) %>%
  summarise(biden_predict =sum(biden_predict_prop)) %>%
  group_by(state) %>%
  summarise(mean =mean(biden_predict),
            lower = quantile(biden_predict, 0.025),
            upper = quantile(biden_predict, 0.975))

write_csv(fullpop_stratified_estimates, "outputs/post_stratified_csv/full_post_stratified_data.csv")