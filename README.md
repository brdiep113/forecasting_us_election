#Overview
This repo contains code and data for forecasting the US 2020 presidential election. It was created by Brian Diep, John Cao, Jonathan Tillmann, and Tanya Ws. The purpose is to create a report that summarises the results of a statistical model that we built. Some data is unable to be shared publicly and we detail how to obtain it below. The sections of this repo are: input, output, scripts.

Inputs contain data that are unchanged from their original source. We use two datasets to build our model:

- [Survey data - To obtain the survey data navigate to "https://www.voterstudygroup.org/publication/nationscape-data-set". Once there type in your information at the bottom of the page and click submit request. You will recieve an email detailing when you can download the data.]
- [ACS data - To obtain the ACS data first register an account at "https://usa.ipums.org". After registering, apply for access of the ACS data and login to the website. Once logged in navigate to Create your custom data set and click "Get Data". Select the 2018 survey. Once done click view cart and then click create data extract. Change the dataset to STATA file type and add a description to the dataset. Once finished click submit extract and download the file once it's ready.]

Outputs contain data that are modified from the input data, the report, and supporting material.

-Model: containing the Bayesian Multilevel Logistic model made with the cleaned data

-post_stratified_csv: containing independent variable data after poststratification

Scripts contain R scripts that take inputs and outputs and produce outputs. These are: 

-01-data_cleaning-survey.R

-01-data_cleaning-post-strat.R
