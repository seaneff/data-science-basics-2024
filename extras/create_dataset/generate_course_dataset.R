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
library(dplyr) ## reshape, reformat, recode data: https://dplyr.tidyverse.org/reference/recode.html
library(readxl) ## to work with Excel files
library(readr) ## to use the convenience function write_delim

#############################################
## Read in data #############################
#############################################

## read in CW vaccine dataset from github
## update once final
#policies <- read.csv("https://raw.githubusercontent.com/cghss/VaxxPolicy/main/ChildhoodVaxxWithIsos.csv?token=GHSAT0AAAAAACPAKVTF6U2UH7SFWMMPYPPIZPDNWKA")

## data downloaded from World Bank on March 2, 2024
## https://databank.worldbank.org/source/population-estimates-and-projections#
## data for the year 2024
population <- read_excel("extras/create_dataset/inputs/population_size.xlsx")

## base data from CIA World Factbook
## reminder: Latin1 to deal with Côte d'Ivoire correctly
base <- read.delim("extras/create_dataset/inputs/locations.tsv", header = TRUE, fileEncoding = "Latin1")

## WHO membership data from WHO
who_membership <- read.delim("extras/create_dataset/inputs/who_member_states.tsv", header = TRUE)

## malaria incidence
malaria_incidence <- read_excel("extras/create_dataset/inputs/malaria_incidence.xls")

## income groups
income_groups <- read_excel("extras/create_dataset/inputs/income_groups.xlsx", sheet = "List of economies")

## regions
regions <- read_excel("extras/create_dataset/inputs/income_groups.xlsx", sheet = "Groups")

#######################################################################
## Process World Bank population count data ###########################
#######################################################################

## rename select values
names(population) <- c("country", "iso_3166", "metric", "metric_code", "value_2021")

## long to wide dataset
population_wide <- population %>% 
  filter(complete.cases(metric)) %>%
  filter(metric != "Age dependency ratio (% of working-age population)") %>%
  select(metric, iso_3166, value_2021) %>%
  filter(iso_3166 != "") %>%
  pivot_wider(id_cols = iso_3166,
              names_from = metric,
              values_from = value_2021)

names(population_wide)[which(names(population_wide) == "Rural population (% of total population)")] <- "pct_rural"
names(population_wide)[which(names(population_wide) == "Urban population (% of total population)")] <- "pct_urban"
names(population_wide)[which(names(population_wide) == "Population, total")] <- "total_population"

#######################################################################
## Process policy data ################################################
#######################################################################

names(policies)[which(names(policies) == "Country")] <- "country"
names(policies)[which(names(policies) == "Subtopic_link")] <- "disease"
names(policies)[which(names(policies) == "Status_link")] <- "measles_policy"
names(policies)[which(names(policies) == "iso_a3_eh")] <- "iso_3166"

measles_policies <- policies %>% filter(disease == "Measles")

#######################################################################
## Process income group data ##########################################
#######################################################################

names(income_groups)[which(names(income_groups) == "income_group_2021")] <- "income_group"

income_groups$income_group <- recode(income_groups$income_group, 
                        "L" = "Low income",
                        "LM" = "Lower middle income",
                        "UM" = "Upper middle income",
                        "H" = "High income")


selected_regions <- regions %>%
  filter(regions$group %in% c("East Asia & Pacific", "Europe & Central Asia",
                     "Latin America & Caribbean", "Middle East & North Africa",
                     "North America", "South Asia", "Sub-Saharan Africa"))

names(selected_regions)[which(names(selected_regions) == "group")] <- "world_bank_region"

#######################################################################
## Process malaria incidence data #####################################
#######################################################################

#############################################
## Merge datasets ###########################
#############################################

full_dataset_1 <- merge(base[,c(1,2)], measles_policies[,c(2,4,6)], by.x = "iso_3166", by.y = "iso_3166", all.x = TRUE, all.y = TRUE)
full_dataset_2 <- merge(full_dataset_1, who_membership[which(who_membership$who_member_state == TRUE),], by.x = "iso_3166", all.x = FALSE, all.y = TRUE)
full_dataset_3 <- merge(full_dataset_2, population_wide, by.x = "iso_3166", all.x = TRUE, all.y = FALSE)
full_dataset_4 <- merge(full_dataset_3, income_groups[,c(1,3)], by = "iso_3166", all.x = TRUE, all.y = FALSE)
full_dataset_5 <- merge(full_dataset_4, selected_regions[,c(2,3)], by = "iso_3166", all.x = TRUE, all.y = FALSE)

#############################################
## Generate course datasets #################
#############################################

day2_dataset <- full_dataset_5[,c(1,2,6,10,11,7,4)]
names(day2_dataset)[which(names(day2_dataset) == "name")] <- "country"

#############################################
## Export day 2 dataset #####################
#############################################

write.table(day2_dataset,
            sep = "\t",
            file = "course-datasets/day2.tsv", 
            na = "NA",
            row.names = FALSE,
            fileEncoding = "Latin1")
