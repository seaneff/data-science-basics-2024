#############################################
## Background ###############################
#############################################

## This script contains code to process raw data into a dataset for in-class use
## original data are archived in the inputs/ subdirectory or are otherwise online
## assume working directory is set to data-science-basics-2024

#############################################
## Setup ####################################
#############################################

## NOTE: if you don't already have these libraries installed, you can do so by running install.packages()
## example: install.packages("dplyr")

## Load libraries
library(tidyr) ## reshape, reformat, recode data: https://dplyr.tidyverse.org/reference/recode.html
library(readxl) ## to work with Excel files
library(readr) ## to use the convenience function write_delim
library(dplyr) ## for data manipulation

#############################################
## Read in data #############################
#############################################

## read in vaccine dataset already created for course
## based on Ciara's work
policies <- read.csv("extras/create_dataset/inputs/measles_policies.csv", fileEncoding = "Latin1")

## data downloaded from World Bank on March 29, 2024
## https://databank.worldbank.org/source/population-estimates-and-projections#
## data for the year 2024
population <- read_excel("extras/create_dataset/inputs/population_size.xlsx")

## base data from CIA World Factbook
## reminder: Latin1 to deal with Côte d'Ivoire correctly
base <- read.delim("extras/create_dataset/inputs/locations.tsv", header = TRUE, fileEncoding = "Latin1")

## WHO membership data from WHO
who_membership <- read.delim("extras/create_dataset/inputs/who_member_states.tsv", header = TRUE)

## measles cases
measles_cases <- read_excel("extras/create_dataset/inputs/measlescasesbycountrybymonth.xlsx", sheet = "WEB")

## income groups
income_groups <- read_excel("extras/create_dataset/inputs/income_groups.xlsx", sheet = "List of economies")

## regions
## ignore weird fifth column
regions <- read_excel("extras/create_dataset/inputs/income_groups.xlsx", sheet = "Groups")[,c(1:4)]

## vaccine coverage (MCV1) from WHO
coverage <- read.csv("extras/create_dataset/inputs/vaccine_coverage.csv")

## our world in data energy data
energy_raw <- read.csv("extras/create_dataset/inputs/owid-energy-data.csv")

#######################################################################
## Process World Bank population count data ###########################
#######################################################################

## rename select values
names(population) <- c("country", "iso_3166", "metric", "metric_code", "value_2024")

## long to wide dataset
population_wide <- population %>% 
  filter(metric %in% c("Population, total", "Rural population (% of total population)")) %>%
  select(metric, iso_3166, value_2024) %>%
  filter(iso_3166 != "") %>%
  pivot_wider(id_cols = iso_3166,
              names_from = metric,
              values_from = value_2024)

names(population_wide)[which(names(population_wide) == "Rural population (% of total population)")] <- "pct_rural"
names(population_wide)[which(names(population_wide) == "Population, total")] <- "total_population"

#######################################################################
## Process income group data ##########################################
#######################################################################

names(income_groups)[which(names(income_groups) == "Income group")] <- "income_group"

income_groups$income_group <- recode(income_groups$income_group, 
                        "L" = "Low income",
                        "LM" = "Lower middle income",
                        "UM" = "Upper middle income",
                        "H" = "High income")

selected_regions <- regions %>%
  filter(regions$GroupName %in% c("East Asia & Pacific", "Europe & Central Asia",
                     "Latin America & Caribbean", "Middle East & North Africa",
                     "North America", "South Asia", "Sub-Saharan Africa"))

names(selected_regions)[which(names(selected_regions) == "GroupName")] <- "world_bank_region"

#############################################
## Rename coverage dataset ##################
#############################################

names(coverage)[which(names(coverage) == "FactValueNumeric")] <- "mcv1_coverage"
names(coverage)[which(names(coverage) == "SpatialDimValueCode")] <- "iso_code"
names(coverage)[which(names(coverage) == "Period")] <- "year"
names(coverage)[which(names(coverage) == "IsLatestYear")] <- "is_latest_year"

#############################################
## Generate countries dataset ###############
#############################################

countries <- merge(base, policies[,c(2,3)], by.x = "iso_code", by.y = "iso_code", all.x = FALSE, all.y = TRUE)
countries <- merge(countries, who_membership[which(who_membership$who_member_state == TRUE),], by.x = "iso_code", by.y = "iso_3166", all.x = FALSE, all.y = TRUE)
countries <- merge(countries, population_wide, by.x = "iso_code", by.y = "iso_3166", all.x = TRUE, all.y = FALSE)
countries <- merge(countries, income_groups[,c(2,4)], by.x = "iso_code", by.y = "Code", all.x = TRUE, all.y = FALSE)
countries <- merge(countries, selected_regions[,c(2,3)], by.x = "iso_code", by.y = "CountryCode", all.x = TRUE, all.y = FALSE)
countries <- merge(countries, coverage[which(coverage$is_latest_year == "true" & coverage$Location.type == "Country"),c(7, 24)], 
                   by.x = "iso_code", by.y = "iso_code", all.x = TRUE, all.y = FALSE)

#######################################################################
## Process measles caseload data ######################################
#######################################################################

measles_long <- measles_cases %>%
  pivot_longer(cols = January:December,
               names_to = "month",
               values_to = "cases") %>%
  mutate(date = as.Date(paste(month, "01,", Year), format = "%B %d, %Y")) 

names(measles_long) <- c("region", "iso_code", "country_name", "year", "month_name", "measles_cases", "month")

measles_long <- merge(measles_long, countries[,c(1,5,8,9)],
                      by = "iso_code", all.x = TRUE, all.y = FALSE)

#######################################################################
## Process final coverage data ########################################
#######################################################################

coverage_temp <- coverage[,c(7, 10, 24, 11)]

coverage_export <- merge(coverage_temp, countries[,c(1,2,5,8,9)],
                      by = "iso_code", all.x = TRUE, all.y = FALSE)

names(coverage_export)[which(names(coverage_export) == "country")] <- "country_name"

coverage_export$is_latest_year[which(coverage_export$is_latest_year == "true")] <- TRUE
coverage_export$is_latest_year[which(coverage_export$is_latest_year == "false")] <- FALSE
coverage_export$is_latest_year <- as.logical(coverage_export$is_latest_year)
  
#######################################################################
## Process energy data ################################################
#######################################################################

full_energy <- energy_raw[which(complete.cases(energy_raw$iso_code) & energy_raw$iso_code != ""),]
energy_2022 <- full_energy[which(full_energy$year == 2022),]

#############################################
## Export datasets ##########################
#############################################

write.table(countries[,c(1,2,5,9,8,6,7,10,3)],
            sep = "\t",
            file = "course-datasets/countries.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")

write.table(measles_long[,c(1,3,7,6,8,10,9)],
            sep = "\t",
            file = "course-datasets/measles_cases.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")

write.table(policies,
            sep = "\t",
            file = "course-datasets/measles_vaccine_policy.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")

write.table(coverage_export[,c(1,5,2,3,6,8,7,4)],
            sep = "\t",
            file = "course-datasets/measles_vaccine_coverage.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")

write.table(energy_2022[,which(names(energy_2022) %in% c("country", "iso_code", "population", "gdp",
                                                         "biofuel_elec_per_capita", "biofuel_electricity", "biofuel_share_elec", 
                                                         "carbon_intensity_elec", "coal_elec_per_capita", "coal_electricity",
                                                         "coal_share_elec", "electricity_demand", "electricity_generation",
                                                         "fossil_elec_per_capita", "fossil_electricity", "fossil_share_elec",
                                                         "gas_elec_per_capita", "gas_electricity", "gas_share_elec", 
                                                         "greenhouse_gas_emissions", "hydro_elec_per_capita", "hydro_electricity",
                                                         "hydro_share_elec", "low_carbon_elec_per_capita", "low_carbon_electricity",
                                                         "low_carbon_share_elec", "net_elec_imports", "net_elec_imports_share_demand",
                                                         "nuclear_elec_per_capita", "nuclear_electricity", "nuclear_share_elec", 
                                                         "oil_elec_per_capita", "oil_electricity", "oil_share_elec", "other_renewable_electricity",
                                                         "other_renewables_elec_per_capita", "per_capita_electricity", "renewables_elec_per_capita",
                                                         "renewables_electricity", "renewables_share_elec", "solar_electricity", "solar_share_elec",
                                                         "wind_elec_per_capita", "wind_electricity", "wind_share_elec"))],
            sep = "\t",
            file = "course-datasets/energy_2022.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")

