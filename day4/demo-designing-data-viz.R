#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2) ## for making plots
library(dplyr) ## for manipulating data
library(camcorder) ## for making gif of figure development

#######################################################################
### Read in course datasets ###########################################
#######################################################################

countries <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/countries.tsv")
measles_cases <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_cases.tsv")
measles_policy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_policy.tsv")
measles_coverage <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_coverage.tsv")

#######################################################################
### View the datasets #################################################
#######################################################################

View(countries)
View(measles_cases)
View(measles_policy)

#######################################################################
### Explore dataset generally #########################################
#######################################################################

## What are the columns/fields?
names(countries)
names(measles_cases)

#######################################################################
### Start recording ###################################################
#######################################################################

gg_record(
  dir = file.path("extras/create_slides/day4_figure_versions_build"),
  device = "png",
  width = 6.4,
  height = 3.6,
  units = "in",
  dpi = 300
)

#######################################################################
### Explore dataset generally #########################################
#######################################################################


## step 1
ggplot(measles_cases, aes(x = month, y = measles_cases, color = income_group)) +
  geom_line()

## step 2: treat month field as a date
measles_cases$month <- as.Date(measles_cases$month)
ggplot(measles_cases, aes(x = month, y = measles_cases, color = income_group)) +
  geom_line()

## step 3: group by country
ggplot(measles_cases, aes(x = month, y = measles_cases, 
                          group = country_name, color = income_group)) +
  geom_line()


## step 4: try faceting
ggplot(measles_cases, aes(x = month, y = measles_cases, 
                          group = country_name, color = income_group)) +
  geom_line() +
  facet_wrap(~income_group)

## step 5: who is missing income group?
## note: I went in and fixed these data, originally Venezuela was missing
unique(measles_cases$country_name[which(is.na(measles_cases$income_group))])
ggplot(measles_cases, aes(x = month, y = measles_cases, 
                          group = country_name, color = income_group)) +
  geom_line() +
  facet_wrap(~income_group)


