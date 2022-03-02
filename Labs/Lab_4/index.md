## Lab 4: Mapping the Ratio of Baltimore Parking Violation and Trees/Shrubs 311 Calls

**Map Description:** 
This is a chloropleth map that shows the ratio of Baltimore parking violation and trees/shrubs 311 calls. This map was created using a join by location to the 311 data, finding the number of 311 calls of each type for each census tract. Another variable containing the ratio of the preceding variables is symbolized on the map using the color scheme.

# Lab 4, Brevin Franklin, GES 486

```{r census api, eval = FALSE}

library(tidycensus);
census_api_key("b6b09fbf43a886071cd481634f74553533bf5d00", overwrite = TRUE)


```



## Exercises 8.6

### 1

```{r Exercises 8.6, 1 code, Getting Data}

library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

# Get MD tract data by race/ethnicity
md_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "MD",
  geometry = TRUE,
  year = 2019
) 

# Use tidycensus to get urbanized areas by population with geometry, 
# then filter for those that have populations of 50,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 50000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", MD Urbanized Area (2010)")))

# Compute an inner spatial join between the MD tracts and the 
# urbanized areas, returning tracts in the largest MD urban 
# areas with the urban_name column appended
md_urban_data <- md_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

```

```{r Exercises 8.6, 1 code, Dissimilarity Index}

# computing dissimilarity index for each major urban area in Maryland where 0 represents perfect integration between the two groups and 1 represents complete segregation

md_disim_index <- md_urban_data %>%
  filter(variable %in% c("white", "black")) %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>% 
  arrange(desc(est))

```

```{r Exercises 8.6, 1 code, Dissimilarity Index for Baltimore}

library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

# filtering the md_urban_data down to just Baltimore area

bal_local_seg <- md_urban_data %>%
  filter(urban_name == "Baltimore") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

```

```{r Exercises 8.6, 1 code, joining to Baltimore tracts}

library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)

# join of Baltimore tracts to Baltimore segregation index

bal_tracts_seg <- tracts("MD", cb = TRUE) %>%
  inner_join(bal_local_seg, by = "GEOID")

bal_tracts_seg %>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26918) + 
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  theme_void() + 
  labs(fill = "White/Black\nsegregation index")


```

### The segregation index in Baltimore does seem to be much different than that for L.A. The most segregated parts of L.A. seem to have a northwest to southeast extent. Black/White segregation in Baltimore seems to have the most segregated parts of the city along the east west extent.

### 2

```{r Exercise 8.6, 2 code, getting Baltimore housing data to run regression}

library(tidycensus)
library(sf)

# the following gets data on housing in Baltimore and Baltimore County as well as the census tracts 

bal_counties <- c("Baltimore County", "Baltimore City")

variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

bal_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "MD",
  county = bal_counties,
  geometry = TRUE,
  output = "wide",
  year = 2019
) %>%
  select(-NAME) %>%
  st_transform(26918) # UTM Zone 18


```

```{r Exercise 8.6, code 2, run regression}

library(sf)
library(units)

# The following code prepares the data for a spatial regression, creating a population density variable and a median structure age variable

bal_data_for_model <- bal_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2019 - median_year_builtE) %>%
  select(!ends_with("M")) %>% 
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()

# the following is the actual code for the regression equation

formula <- "log(median_value) ~ median_rooms + median_income + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model1 <- lm(formula = formula, data = bal_data_for_model)

summary(model1)

# Then, we store the residuals from the equation in an object

bal_data_for_model$residuals <- residuals(model1)
```

<img src="images/dummy_thumbnail.jpg?raw=true"/>

