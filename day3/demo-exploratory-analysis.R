#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2)
library(dplyr)

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
View(cases)

#######################################################################
### Explore dataset generally #########################################
#######################################################################

## What are the columns/fields?
names(countries)
names(cases)

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
### Data visualization: ################################################################################
### Barplot (ggplot2) ##################################################################################
########################################################################################################

## most basic possible scatterplot in ggplot2
ggplot(data = countries, aes(who_region)) +
  geom_bar()

## labels are hard to see, try flipping plot
ggplot(data = countries, aes(who_region)) +
  geom_bar() +
  ## reverse the X and Y coordinates (flip barplot horizontally)
  coord_flip()

## add colors and titles
ggplot(data = countries, aes(who_region)) +
  ## fill is the color used to fill the barplot, black is the color that surrounds the bars
  geom_bar(fill = "dark blue", color = "black") +
  ylab("Count") +
  xlab("") +
  ggtitle("Number of WHO member states\nper WHO region") +
  coord_flip() 

########################################################################################################
### Data visualization: ################################################################################
### Stacked barplot (ggplot2) ##########################################################################
########################################################################################################

sanctions %>%
  filter(primary_country == "DPRK") %>%
  ggplot(aes(x = primary_sanctions_program, group = type, fill = type)) +
  geom_bar(position = "stack", color = "black") +
  xlab("Primary sanctions program") +
  ylab("Number of sanctions") +
  labs(fill = "Sanction type",
       ## you can add a caption by specifying caption from within the labs() function
       caption = "Based on public data from\nUS Office of Foreign Assets Control as of June 2023") +
  ggtitle("Types of sanctions in DPRK") 

########################################################################################################
### Data visualization: ################################################################################
### Boxplot (ggplot2) ##################################################################################
########################################################################################################

## most basic possible boxplot in ggplot2
breeds %>%
  filter(is_most_recent_with_data == TRUE) %>%
  ggplot(aes(x = proportion_at_risk, y = who_region)) +
  geom_boxplot()

## add colors and titles
breeds %>%
  filter(is_most_recent_with_data == TRUE) %>%
  ggplot(aes(x = proportion_at_risk, y = who_region)) +
  geom_boxplot(fill = "#2E8A99") +
  xlab("Proportion of species at risk") +
  ylab("") +
  ggtitle("Proportion of species at risk by region") 

## what if I want a percent along the x axis?
breeds %>%
  filter(is_most_recent_with_data == TRUE) %>% 
  ggplot(aes(x = proportion_at_risk/100, y = who_region)) + ## divide proportion at risk by 100
  geom_boxplot(fill = "#2E8A99") +
  xlab("Percentage of species at risk") +
  ylab("") +
  ggtitle("Percent of species at risk by region") +
  scale_x_continuous(labels = scales::percent) ## tell R to scale the x axis based on a percentage

########################################################################################################
### Data visualization: ################################################################################
### World maps #########################################################################################
########################################################################################################

## read in and format data
world <- map_data("world")
world_data <- inner_join(world, countries, by = join_by(region == country_name))

## generate world map plot
ggplot(data = world_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = mds_per_10000capita)) +
  scale_fill_distiller(direction = 1) +
  ggtitle("Health workforce per capita") +
  labs(fill = "MDs per\n10,000 capita") +
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
### Data visualization: ################################################################################
### Cleveland dot plot #################################################################################
### Getting fancier here, you don't need to replicaate this one ########################################
########################################################################################################

# see more at https://r-graph-gallery.com/303-lollipop-plot-with-2-values.html

countries %>%
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
