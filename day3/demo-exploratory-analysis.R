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

#############################################################################################
### Descriptive statistics: #################################################################
### Measures of centrality (average, median) ################################################
#############################################################################################

## average, across countries reporting data, of proportion of total population who live in a rural area
mean(countries$pct_rural) ## what happened here? missing values
mean(countries$pct_rural, na.rm = TRUE) 

## median, across countries reporting data, of proportion of total population who live in a rural area
median(countries$pct_rural) ## what happened here? missing values
median(countries$pct_rural, na.rm = TRUE) 

## look into missing data
table(is.na(countries$pct_rural))

#############################################################################################
### Descriptive statistics: #################################################################
### Measures of spread (standard deviation, interquartile range) ############################
#############################################################################################

## standard deviation, across countries reporting data, of MCV1 rates
## MCV1 =  Measles-containing-vaccine first-dose (MCV1) immunization coverage among 1-year-olds (%) (WUENIC) 
sd(countries$mcv1_coverage) 

## interquartile range (IQR), across countries reporting data, of MVC1 rates
quantile(countries$mcv1_coverage) 

#############################################################################################
### Descriptive statistics: #################################################################
### Counts and rates ########################################################################
#############################################################################################

## counts of countries in each WHO region
table(countries$who_region)

## number of countries where over 75% of the population live in a rural area
table(countries$pct_rural > 75) 

## which countries have over 75% of the population live in a rural area?
countries[which(countries$pct_rural > 75),]$country

#############################################################################################
### Descriptive statistics ##################################################################
### by group ################################################################################
#############################################################################################

countries %>%
  group_by(who_region) %>%
  summarize(n = n(),
            avg_vaccination_rate = mean(mcv1_coverage),
            sd_vaccination_rate = mean(mcv1_coverage))

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## Using the code above, instead of calculating average vaccination rate per WHO region, 
## calculate the averagn population size per income group

########################################################################################################
### Data visualization: ################################################################################
### Histogram (base R) #################################################################################
########################################################################################################

## most basic possible histogram
hist(countries$mcv1_coverage)

## full version of a histogram
hist(countries$mcv1_coverage,
     ## xlab specifies the x axis label, the \n here tells R to make a line break
     xlab = "Percent of 1-year olds who have\nreceived at least one measles vaccine",
     ## ylab specifies the y axis label
     ylab = "Number of countries",
     ## main specifies the primary title, the \n here tells R to make a line break
     main = "Distribution of country-level\nmeasles vaccination rates\nfor 1-year olds (MCV1)",
     ## specify the color of the bars
     col = "light blue",
     ## specify the number of bins
     breaks = 20)

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## change the color of the plot above
## examples of colors here, or you can use hex codes: https://r-charts.com/colors/

########################################################################################################
### Data visualization: ################################################################################
### Boxplot (base R) ###################################################################################
########################################################################################################

## most basic possible boxplot
boxplot(countries$mcv1_coverage)

## full version of a boxplot
boxplot(countries$mcv1_coverage,
     ## xlab specifies the x axis label, the \n here tells R to make a line break
     xlab = "Percent of 1-year olds who have\nreceived at least one measles vaccine",
     ## ylab specifies the y axis label
     ylab = "",
     horizontal = TRUE,
     ## main specifies the primary title, the \n here tells R to make a line break
     main = "Distribution of country-level\nmeasles vaccination rates\nfor 1-year olds (MCV1)",
     ## specify a fill color using a hex code 
     col = "#77B0AA")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## change the plot above to be vertically oriented
## (horizontal = FALSE)
## what else do you need to change now?

########################################################################################################
### Data visualization: ################################################################################
### Bar chart (base R) #################################################################################
########################################################################################################

## most basic possible bar chart
barplot(table(countries$income_group))

## but we want income group to be sorted! (we'll learn more about factors later)
countries$income_group <- factor(countries$income_group, levels = c("Low income",
                                                                    "Lower middle income",
                                                                    "Upper middle income",
                                                                    "High income"))

## now try again
barplot(table(countries$income_group))

## and add some color/styling
barplot(table(countries$income_group),
        main = "Number of WHO member states\nper income group",
        xlab = "",
        ylab = "Count",
        col = "#005879")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## add a box around the plot by uncommenting and running the command below
# box()

########################################################################################################
### Data visualization: ################################################################################
### Pie chart (base R) #################################################################################
########################################################################################################

## most basic possible pie chart
pie(table(countries$measles_vaccine_policy))

## full pie chart
pie(table(countries$measles_vaccine_policy),
    col = c("gray80","#256d85", "#f49431"),
    ## add a title
    main = "Policy requirement for\nmeasles vaccination")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## remove the line break from the title
## (remove\n)

########################################################################################################
### Data visualization: ################################################################################
### Scatterplot (base R) ###############################################################################
########################################################################################################

## most basic possible scatterplot
plot(x = countries$pct_rural,
     y = countries$mcv1_coverage)

## full scatterplot
plot(x = countries$pct_rural,
     y = countries$mcv1_coverage,
     pch = 1,
     xlab = "Percent of population in rural areas",
     ylab = "Vaccination rate (MCV1)",
     main = "Measles vaccination rates\nfor 1-year olds (MCV1)\nvs. percent of population in rural areas",
     col = "#8B4F80")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## change the type of point (pch) and the color of the points (using a hex code)
## example pch values: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
## example hex codes: https://coolors.co/palettes/trending 
## make sure to add the "#" before your hex code

########################################################################################################
### Data visualization: ################################################################################
### Histogram (ggplot2) ################################################################################
########################################################################################################

## most basic possible histogram in ggplot2
ggplot(data = countries, aes(mcv1_coverage)) +
  geom_histogram()

## add axis labels and a title, adjust binwidth
ggplot(data = countries, aes(mcv1_coverage)) +
  geom_histogram(binwidth = 5, fill = "light blue", color = "black") +
  ## xlab specifies the x axis label
  xlab("Percent of population") +
  ## ylab specifies the x axis label
  ylab("Number of countries") +
  ## ggtitle specifies the title
  ggtitle("Distribution of country-level measles vaccination rates\nfor 1-year olds (MCV1)") +
  ## add a caption
  labs(caption = "MCV1 is defined as the percentage of children under one year of age who have\nreceived at least one dose of measles-containing vaccine in a given year.")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## remove or change the caption

########################################################################################################
### Data visualization: ################################################################################
### Boxplot (ggplot2) ##################################################################################
########################################################################################################

## full version of a boxplot
ggplot(data = countries, aes(mcv1_coverage/100)) +
  geom_boxplot(fill = "#77B0AA") +
  ## xlab specifies the x axis label
  xlab("Percent of 1-year olds who have\nreceived at least one measles vaccine") +
  ## ylab specifies the y axis label
  ylab("") + 
  ## ggtitle specifies the title
  ggtitle("Distribution of country-level measles vaccination rates\nfor 1-year olds (MCV1)") +
  ## remove numbers from the y axis
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ## scale x axis as percentage
  scale_x_continuous(labels = scales::percent_format())

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## comment out the theme() code above and see what happens

########################################################################################################
### Data visualization: ################################################################################
### Boxplot by group (ggplot2) #########################################################################
########################################################################################################

## full version of a boxplot
countries %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(x = mcv1_coverage/100, 
             y = income_group,
             group = income_group)) +
  geom_boxplot(fill = "#77B0AA") +
  ## xlab specifies the x axis label
  xlab("Percent of 1-year olds who have\nreceived at least one measles vaccine") +
  ## ylab specifies the y axis label
  ylab("") + 
  ## ggtitle specifies the title
  ggtitle("Distribution of country-level measles vaccination rates\nfor 1-year olds (MCV1) by income group") +
  ## scale x axis as percentage
  scale_x_continuous(labels = scales::percent_format())

########################################################################################################
### Data visualization: ################################################################################
### Scatterplot (ggplot2) ##############################################################################
########################################################################################################

## most basic possible scatterplot in ggplot2
ggplot(data = countries, aes(x = pct_rural, y = mcv1_coverage)) +
  geom_point()

ggplot(data = countries, aes(x = pct_rural/100, y = mcv1_coverage/100)) +
  ## plot points
  geom_point(color = "#8B4F80") +
  ## xlab specifies the x axis label
  xlab("Percent of population in rural areas") +
  ## ylab specifies the y axis label
  ylab("Vaccination rate (MCV1)") +
  ## ggtitle specifies the main title
  ggtitle("Measles vaccination rates for 1-year olds (MCV1)\nvs. percent of population in rural areas") +
  ## scale x axis as percentage
  scale_x_continuous(labels = scales::percent_format()) +
  ## scale y axis as percentage
  scale_y_continuous(labels = scales::percent_format())

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## change the axis labels to numbers without the % symbol
## what do you need to comment out or delete?

########################################################################################################
### Data visualization: ################################################################################
### Barplot (ggplot2) ##################################################################################
########################################################################################################

## most basic possible scatterplot in ggplot2
ggplot(data = countries, aes(income_group)) +
  geom_bar()

## labels are hard to see, try flipping plot
ggplot(data = countries, aes(income_group)) +
  geom_bar() +
  ## reverse the X and Y coordinates (flip barplot horizontally)
  coord_flip()

## excluding missing values from plot
## dont worry about the changing ggplot2 syntax for now
countries %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(income_group)) +
  geom_bar() +
  ## reverse the X and Y coordinates (flip barplot horizontally)
  coord_flip()

## add colors and titles
countries %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(income_group)) +
  ## fill is the color used to fill the barplot, black is the color that surrounds the bars
  geom_bar(fill = "#005879", color = "black") +
  ylab("Count") +
  xlab("") +
  ggtitle("Number of WHO member states\nper income group") +
  coord_flip() 

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## add a y axis label to the code above

########################################################################################################
### Discussion #########################################################################################
########################################################################################################

## what is this plot showing?
## what might make it more informative or interesting?

## filled (instead of stacked) barchart
countries %>%
## don't worry about these mutate commands too much for now, they let us update the order of the
## colors and the things on the y axis
mutate(measles_vaccine_policy = factor(measles_vaccine_policy,
                                       levels = c("no data", "not required", "required"))) %>%
mutate(who_region = factor(who_region,
                           levels = c("South-East Asia Region",
                                      "African Region",
                                      "Western Pacific Region",
                                      "European Region",
                                      "Eastern Mediterranean Region",
                                      "Region of the Americas"))) %>%
ggplot(aes(y = who_region, group = measles_vaccine_policy, 
           fill = measles_vaccine_policy)) +
  geom_bar(position = "fill") +
  labs(x = "Percent of countries",
       y = "",
       fill = "Measles vaccine policy?",
       title = "Measles vaccine policies by WHO region") +
  ## this makes the x axisshow a percentage
  scale_x_continuous(labels = scales::label_percent()) +
  ## this specifies the colors for the fill values
  scale_fill_manual(values = c( "#89ac53", "#6c7698", "gray80"),
                    breaks = c("required", "not required", "no data")) 

########################################################################################################
### Data visualization: ################################################################################
### Line chart #########################################################################################
########################################################################################################

## generic line plot
measles_cases %>%
  filter(country_name == "Afghanistan") %>%
  ggplot(aes(x = month, y = measles_cases, group = country_name)) +
  geom_line()

## why is the x-axis weird?
is(measles_cases$month)

## tell R to treat the data as a date
measles_cases$month <- as.Date(measles_cases$month)

## try again
measles_cases %>%
  filter(country_name == "Afghanistan") %>%
  ggplot(aes(x = month, y = measles_cases, group = country_name)) +
  geom_line()

## add styling
measles_cases %>%
  filter(country_name == "Afghanistan") %>%
  ggplot(aes(x = month, y = measles_cases, group = country_name)) +
  geom_line(color = "#3d4d6d") +
  labs(x = "",
       y = "Measles cases per month",
       title = "Measles cases per month in Afghanistan")

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## make the plot above for another country
## and use a different color line

########################################################################################################
### Data visualization: ################################################################################
### World maps #########################################################################################
########################################################################################################

## read in and format data
world <- map_data("world")
world_data <- inner_join(world, countries, by = join_by(region == country))

## generate world map plot
ggplot(data = world_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = who_region)) +
  #scale_fill_distiller(direction = 1) +
  ggtitle("WHO regions") +
  labs(fill = "WHO regions") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

########################################################################################################
### Your turn ##########################################################################################
########################################################################################################

## the plot above isn't very informative
## change the colors to show some data that would teach us something useful about our dataset

########################################################################################################
### Data visualization: ################################################################################
### World maps #########################################################################################
########################################################################################################

## generate world map plot
ggplot(data = world_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = measles_vaccine_policy)) +
  ggtitle("Measles vaccine policy requirements") +
  labs(fill = "Measles vaccine") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)) +
    ## this specifies the colors for the fill values
    scale_fill_manual(values = c( "#89ac53", "#6c7698", "gray80"),
                      breaks = c("required", "not required", "no data")) 

########################################################################################################
### Data visualization: ################################################################################
### Cleveland dot plot #################################################################################
### Getting fancier here, you don't need to replicaate this one ########################################
########################################################################################################

crime <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/additional_example_crime.tsv")

crime %>%
  filter(complete.cases(safe_after_dark_female)) %>%
  filter(complete.cases(safe_after_dark_male)) %>%
  filter(who_region == "Region of the Americas") %>%
  mutate(country_name = factor(country_name, levels = country_name[order(safe_after_dark_female[complete.cases(safe_after_dark_female)])])) %>%
  ggplot() +
  geom_segment( aes(y = country_name, yend = country_name,
                    x = safe_after_dark_female/100, xend = safe_after_dark_male/100), color = "grey") +
  geom_point( aes(y = country_name, x = safe_after_dark_female/100), color = "#22A699", size = 3 ) +
  geom_point( aes(y = country_name, x = safe_after_dark_male/100), color = "#F29727", size = 3 ) +
  xlab("Percentage of people") +
  ylab("") +
  ggtitle("Percentage of people who feel\nsafe walking alone after dark") +
  scale_x_continuous(labels = scales::percent)
  
########################################################################################################
### Data visualization: ################################################################################
### Study trajectory plot (line) with error bars #######################################################
### Getting fancier here, you don't need to replicate this one #########################################
########################################################################################################

sample_study_data <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/additional_example_study.tsv")

ggplot(sample_study_data, aes(x = time, y = average_score, col = group, group = group,
                              ymin = ci_lower, ymax = ci_upper)) +
  ## below, the position_dodge argument helps offset the points so they don't totally overlap
  geom_point(position = position_dodge(width = 0.1)) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.1),
                linetype = "longdash") +
  xlab("") +
  ylab("Average score") +
  ggtitle("Example study trajectory plot") +
  theme_light() +
  theme(plot.caption = element_text(size = 8)) +
  labs(caption = "Data are notional and do not reflect actual study data",
       color = "Group") +
  scale_color_manual(values = c("#0C356A", "#279EFF"))

########################################################################################################
### Data visualization: ################################################################################
### Study trajectory plot (bar) with error bars ########################################################
### Getting fancier here, you don't need to replicate this one #########################################
########################################################################################################

ggplot(study, aes(x = time, y = average_score, fill = group, group = group,
                              ymin = ci_lower, ymax = ci_upper)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(width = .4, position = position_dodge(.9), 
                linetype = "longdash") +
  xlab("") +
  ylab("Average score") +
  ggtitle("Example study trajectory plot") +
  theme_light() +
  theme(plot.caption = element_text(size = 8)) +
  labs(caption = "Data are notional and do not reflect actual study data",
       fill = "Group") +
  scale_fill_manual(values = c("#0C356A", "#279EFF"))
