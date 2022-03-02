## Lab 3: Temperature and the Elderly

**Project description:** 
This is a chloropleth map of the number of veterans in Virginia census tracts based off 2019 ACS data. The code is meant to create an interactive hmtl page of the map.

### R Code 

```{r  Exercise 6.8, 2 plotting points part 1}

# Loading packages from library and getting 2019 5-year ACS data on the number of people who are veterans in the D.C. metropolitan area (DMV - DC, MD, and VA)

library(tidycensus)
library(shiny)
library(leaflet)
library(tidyverse)

census_api_key("b6b09fbf43a886071cd481634f74553533bf5d00", overwrite = TRUE)

# getting data on veterans in D.C., Maryland, and Virgina

dmv_workers <- get_acs(
  geography = "tract",
  variables = "B21003_002",
  state = c("VA", "MD", "DC"),
  year = 2019,
  survey = "acs5",
  geometry = TRUE,
)

# Creating the interactive map

us_pal <- colorNumeric(
  palette = "plasma",
  domain = dmv_workers$estimate
)

vet_map <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = dmv_workers,
              color = ~us_pal(estimate),
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5,
              label = ~estimate) %>%
  addLegend(
    position = "bottomright",
    pal = us_pal,
    values = dmv_workers$estimate,
    title = "Number of Vetrans"
  )

# Saving the map as html
library(htmlwidgets)

saveWidget(vet_map, "vet_map.html", selfcontained = TRUE)




```
