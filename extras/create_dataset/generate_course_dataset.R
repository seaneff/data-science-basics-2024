#############################################
## Background ###############################
#############################################

## This script contains code to process raw data into a dataset for in-class use
## original data are archived in the inputs/ subdirectory
## assume working directory is set to data-science-basics-2024

#############################################
## Setup ####################################
#############################################

## NOTE: if you don't already have these libraries installed, you can do so by running install.packages()
## example: install.packages("dplyr")

## Load libraries
library(dplyr) ## reshape, reformat, recode data: https://dplyr.tidyverse.org/reference/recode.html
library(readxl) ## to work with Excel files
library(readr) ## to use the convenience function write_delim

#############################################
## Read in data #############################
#############################################

## goal 3
## data downloaded from UNSD SDG Data Download on January 9, 2024
goal3 <- read_excel("extras/create_dataset/inputs/Goal3.xlsx")

## data downloaded from World Bank on January 19, 2024
## https://databank.worldbank.org/source/population-estimates-and-projections#
## data for the year 2021
population <- read_excel("extras/create_dataset/inputs/population_size.xlsx")

## base data from CIA World Factbook
## reminder: Latin1 to deal with Côte d'Ivoire correctly
base <- read.delim("extras/create_dataset/inputs/locations.tsv", header = TRUE, fileEncoding = "Latin1")

## WHO membership data from WHO
who_membership <- read.delim("extras/create_dataset/inputs/who_member_states.tsv", header = TRUE)

#############################################
## Filter and rename UNSDG goal data ########
#############################################

## select our columns
goal3_export <- goal3[## identify the rows we want
                      which(complete.cases(as.numeric(goal3$Time_Detail)) & 
                      (!goal3$GeoAreaName %in% c("Netherlands Antilles  [former]", "Serbia and Montenegro [former]")) & 
                      (is.na(goal3$Sex) | goal3$Sex %in% c("MALE", "FEMALE")) &
                      goal3$SeriesDescription %in% c("Maternal mortality ratio", "Proportion of births attended by skilled health personnel (%)",
                                                                    "Infant mortality rate (deaths per 1,000 live births)", "Infant deaths (number)",
                                                                    "Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)",
                                                                    "Tuberculosis incidence (per 100,000 population)", "Malaria incidence per 1,000 population at risk (per 1,000 population)",
                                                                    "Universal health coverage (UHC) service coverage index", 
                                                                    "Proportion of population with large household expenditures on health (greater than 10%) as a share of total household expenditure or income (%)",
                                                                    "Age-standardized mortality rate attributed to ambient air pollution (deaths per 100,000 population)",
                                                                    "Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population)",
                                                                    "Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population)")),
                      ## identify the columns we want
                      which(names(goal3) %in% c("SeriesDescription", "GeoAreaName", "Time_Detail", "Value", "UpperBound", "LowerBound", "Source"))]

names(goal3_export)[which(names(goal3_export) == "SeriesDescription")] <- "metric"
names(goal3_export)[which(names(goal3_export) == "GeoAreaName")] <- "country"
names(goal3_export)[which(names(goal3_export) == "Time_Detail")] <- "year"
names(goal3_export)[which(names(goal3_export) == "Value")] <- "value"
names(goal3_export)[which(names(goal3_export) == "LowerBound")] <- "lower_bound"
names(goal3_export)[which(names(goal3_export) == "UpperBound")] <- "upper_bound"
names(goal3_export)[which(names(goal3_export) == "Source")] <- "source"

#######################################################################
## Process World Bank population count data ###########################
#######################################################################

## rename select values
names(population) <- c("country", "iso_3166", "metric", "metric_code", "value_2021")

## long to wide dataset
population_wide <- population %>% 
  filter(complete.cases(metric)) %>%
  select(metric, iso_3166, value_2021) %>%
  pivot_wider(id_cols = iso_3166,
              names_from = metric,
              values_from = value_2021)

names(population_wide) <- c("iso_3166", "age_dependency_ratio", "total_population", "pct_rural", "pct_urban")

#############################################
## Clean country names/ISOs #################
#############################################

if(any(goal3_export$country == "Bolivia (Plurinational State of)")){
  goal3_export$country[which(goal3_export$country == "Bolivia (Plurinational State of)")] <- "Bolivia"
}

if(any(goal3_export$country == "Brunei Darussalam")){
  goal3_export$country[which(goal3_export$country == "Brunei Darussalam")] <- "Brunei"
}

if(any(goal3_export$country == "Brunei Darussalam")){
  goal3_export$country[which(goal3_export$country == "Brunei Darussalam")] <- "Brunei"
}

if(any(base$name == "Congo, Democratic Republic of the")){
  base$name[which(base$name == "Congo, Democratic Republic of the")] <- "Democratic Republic of the Congo"
}

if(any(base$name == "Congo, Republic of the")){
  base$name[which(base$name == "Congo, Republic of the")] <- "Congo"
}

if(any(goal3_export$country == "State of Palestine")){
  goal3_export$country[which(goal3_export$country == "State of Palestine")] <- "Palestine"
}

if(any(goal3_export$country == "Iran (Islamic Republic of)")){
  goal3_export$country[which(goal3_export$country == "Iran (Islamic Republic of)")] <- "Iran"
}

if(any(goal3_export$country == "Democratic People's Republic of Korea")){
  goal3_export$country[which(goal3_export$country == "Democratic People's Republic of Korea")] <- "North Korea"
}

if(any(goal3_export$country == "Republic of Korea")){
  goal3_export$country[which(goal3_export$country == "Republic of Korea")] <- "South Korea"
}

if(any(goal3_export$country == "Lao People's Democratic Republic")){
  goal3_export$country[which(goal3_export$country == "Lao People's Democratic Republic")] <- "Laos"
}

if(any(goal3_export$country == "Republic of Moldova")){
  goal3_export$country[which(goal3_export$country == "Republic of Moldova")] <- "Moldova"
}

if(any(goal3_export$country == "Netherlands (Kingdom of the)")){
  goal3_export$country[which(goal3_export$country == "Netherlands (Kingdom of the)")] <- "Netherlands"
}

if(any(goal3_export$country == "Micronesia (Federated States of)")){
  goal3_export$country[which(goal3_export$country == "Micronesia (Federated States of)")] <- "Micronesia"
}

if(any(base$name == "Micronesia, Federated States of")){
  base$name[which(base$name == "Micronesia, Federated States of")] <- "Micronesia"
}

if(any(base$name == "Russian Federation")){
  base$name[which(base$name == "Russian Federation")] <- "Russia"
}

if(any(goal3_export$country == "Russian Federation")){
  goal3_export$country[which(goal3_export$country == "Russian Federation")] <- "Russia"
}

if(any(goal3_export$country == "Viet Nam")){
  goal3_export$country[which(goal3_export$country == "Viet Nam")] <- "Vietnam"
}

if(any(goal3_export$country == "Syrian Arab Republic")){
  goal3_export$country[which(goal3_export$country == "Syrian Arab Republic")] <- "Syria"
}

if(any(goal3_export$country == "Türkiye")){
  goal3_export$country[which(goal3_export$country == "Türkiye")] <- "Turkey"
}

if(any(goal3_export$country == "United Kingdom of Great Britain and Northern Ireland")){
  goal3_export$country[which(goal3_export$country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
}

if(any(goal3_export$country == "United Republic of Tanzania")){
  goal3_export$country[which(goal3_export$country == "United Republic of Tanzania")] <- "Tanzania"
}

if(any(base$name == "United States")){
  base$name[which(base$name == "United States")] <- "United States of America"
}

if(any(goal3_export$country == "Venezuela (Bolivarian Republic of)")){
  goal3_export$country[which(goal3_export$country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
}

if(any(goal3_export$country == "China, Hong Kong Special Administrative Region")){
  goal3_export$country[which(goal3_export$country == "China, Hong Kong Special Administrative Region")] <- "Hong Kong"
}

if(any(goal3_export$country == "China, Macao Special Administrative Region")){
  goal3_export$country[which(goal3_export$country == "China, Macao Special Administrative Region")] <- "Macau"
}

if(any(goal3_export$country == "Sint Maarten (Dutch part)")){
  goal3_export$country[which(goal3_export$country == "Sint Maarten (Dutch part)")] <- "Sint Maarten"
}

if(any(goal3_export$country == "Sint Maarten (Dutch part)")){
  goal3_export$country[which(goal3_export$country == "Sint Maarten (Dutch part)")] <- "Sint Maarten"
}

if(any(base$name == "Wallis and Futuna")){
  base$name[which(base$name == "Wallis and Futuna")] <- "Wallis and Futuna Islands"
}

#############################################
## Merge datasets ###########################
#############################################

full_dataset_1 <- merge(base, goal3_export, by.x = "name", by.y = "country", all.x = FALSE, all.y = TRUE)
full_dataset_2 <- merge(full_dataset_1, who_membership, by.x = "iso_3166", all.x = TRUE, all.y = FALSE)
full_dataset_3 <- merge(full_dataset_2, population_wide, by.x = "iso_3166", all.x = TRUE, all.y = FALSE)

#############################################
## Export simple day 1 dataset ##############
#############################################

day2_dataset <- full_dataset_3 %>%
  filter(metric == "Malaria incidence per 1,000 population at risk (per 1,000 population)") %>%
  filter(year == 2021) %>%
  filter(name != "Mayotte") %>% ## no population data
  select(iso_3166, name, who_member_state, who_region, total_population, pct_rural, pct_urban, value)

names(day2_dataset)[which(names(day2_dataset) == "value")] <- "malaria_incidence"
names(day2_dataset)[which(names(day2_dataset) == "name")] <- "location"

#############################################
## Export day 2 dataset #####################
#############################################

write.table(day2_dataset,
            sep = "\t",
            file = "course-datasets/day2.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")
