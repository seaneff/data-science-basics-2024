#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(maps)

#######################################################################
### Read in course datasets ###########################################
#######################################################################

countries <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/countries.tsv")
measles_cases <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_cases.tsv")
measles_policy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_policy.tsv")
measles_coverage <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_coverage.tsv")

#######################################################################
### Policy plot #######################################################
#######################################################################

policy_bar <- measles_policy %>%
  ggplot(aes(measles_vaccine_policy)) +
  geom_bar(fill = "#0E6D85", col =  "#0E6D85") +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

ggsave(policy_bar, width = 5, height = 2, units = "cm", file = "extras/create_slides/policy_bar.png", bg = 'transparent')

#######################################################################
### Coverage plot #####################################################
#######################################################################

measles_coverage$date <- as.Date(paste(measles_coverage$year, "/01/01", sep = ""))

measles_line <- measles_coverage %>%
  filter(country_name == "Afghanistan") %>%
  ggplot(aes(x = date, y = mcv1_coverage)) +
  geom_line(col = "#006DA3", lwd = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

ggsave(measles_line, width = 5, height = 2, units = "cm", file = "extras/create_slides/measles_line.png", bg = 'transparent')

#######################################################################
### Caseload plot #####################################################
#######################################################################

cases <- measles_cases %>%
  filter(country_name == "Afghanistan") %>%
  ggplot(aes(x = month, y = measles_cases, group = 1)) +
  geom_line(col = "#864E93", lwd = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

ggsave(cases, width = 5, height = 2, units = "cm", file = "extras/create_slides/measles_cases.png", bg = 'transparent')

