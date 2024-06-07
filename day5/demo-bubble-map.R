#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("dplyr")
## The code above will install the package you need on your computer
## you only need to run it once, if you haven't downloaded the package before

## within any new R session, you need to load the libraries you need
library(dplyr)
library(scales)
library(forcats)
library(sf)

#######################################################################
### Read in course datasets ###########################################
#######################################################################

## borrowing from UNHCR online code here: https://dataviz.unhcr.org/tools/r/r_bubble_map.htmls
df_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/bubble_map.csv"
poly_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_polygons_simplified.json"
line_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_lines_simplified.json"
countries <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/countries.tsv")
measles_cases <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_cases.tsv")
measles_policy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_policy.tsv")
measles_coverage <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_coverage.tsv")
measles_big <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_coverage_cases_gdp.tsv")

#######################################################################
### Reformat data #####################################################
#######################################################################

# Read and transform data
df <- read_csv(df_url) |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

poly <- read_sf(poly_url) |> 
  st_set_crs(4326)

line <- read_sf(line_url) |>
  mutate(
    type = as_factor(type) |>
      fct_relevel("solid", "dashed", "dotted", "dashed-dot")
  ) |> 
  st_set_crs(4326)

#######################################################################
### Add in data on measles to the GIS info we need to map #############
#######################################################################

bubble_data <- merge(df, measles_big, by.x = "iso3", by.y = "iso_code")

#######################################################################
### Try out a plot ####################################################
#######################################################################

# Plot
ggplot() +
  geom_sf(data = poly,
          fill = "gray80",
          color = "transparent") +
  geom_sf(data = line,
          aes(linetype = type),
          color = "white",
          linewidth = .25,
          show.legend = FALSE) +
  geom_sf(data = bubble_data,
          aes(size = mcv1_coverage),
          shape = 21,
          # fill = unhcr_pal(n = 1, "pal_blue"),
          #  color = unhcr_pal(n = 5, "pal_blue")[5],
          alpha = 0.3) 
scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_size_area(max_size = 12,
                  labels = scales::label_number(
                    scale_cut = cut_short_scale()
                  ),
                  breaks = c(1e5, 1e6, 5e6)) +
  labs(
    title = "Add title",
    caption = "Add caption"
  ) +
  coord_sf(crs = st_crs('ESRI:54030')) 