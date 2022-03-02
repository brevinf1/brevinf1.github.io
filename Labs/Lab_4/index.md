## Lab 4: Mapping the Ratio of Baltimore Parking Violation and Trees/Shrubs 311 Calls

**Map Description:** 
This is a chloropleth map that shows the ratio of Baltimore parking violation and trees/shrubs 311 calls. This map was created using a join by location to the 311 data, finding the number of 311 calls of each type for each census tract. Another variable containing the ratio of the preceding variables is symbolized on the map using the color scheme.

### R Code

```{r, reading in 311 data, results = FALSE}
library(tidyr)

# The following reads in the Baltimore 311 data from a csv file

bal_city_data <- read.csv("311_Customer_Service_Requests_2020.csv", header = TRUE)

```

```{r, preparing data, results = FALSE}
library(dplyr)

# The following separates the column containing request type so that it can be easily manipulated. 

bal_city_data <- separate(bal_city_data, srtype, c("code", "issue"), sep = "-")

```

```{r, renaming variables, results = FALSE}

# The following renames the variables.


bal_city_parking <- filter(bal_city_data, issue == "Parking Complaint")

bal_city_trees <- filter(bal_city_data, issue == "Trees and Shrubs")



```



```{r, converting sf objects, results = FALSE}

library(sf)

# The following converts the seperate files for the parking and tree/shrub data, defines them as SF objects, and defines the geographic coordinate system. It then transforms the coordinate system to NAD83 UTM Zone 18.  

bal_city_parking_sf <- bal_city_parking%>%
  mutate_at(vars(longitude, latitude), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 4267,       
    stringsAsFactors = FALSE,
    remove = TRUE
    )  %>% 
  st_transform(26918) %>%
  select(ï..objectid:policepost, geometry)

bal_city_trees_sf <- bal_city_trees%>%
  mutate_at(vars(longitude, latitude), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 4267,       
    stringsAsFactors = FALSE,
    remove = TRUE
    ) %>% 
  st_transform(26918) %>%
  select(ï..objectid:policepost, geometry)




```

```{r, determine coord. sys., eval = FALSE}

library(crsuggest)
# for determining the coordinate system

guess_crs(bal_city_parking_sf, c(-76.63399189, 39.26137126), units = NULL, n_return = 10)

# Find that the data are in NAD27 geographic coordinate system.

```


```{r, Bal Tracts from census, results = FALSE}

library(tigris)

# The following gets the shapefiles for Baltimore City census tracts.

bal_tracts <- tracts(state = "MD", county = "Baltimore City", year = "2019") %>%
  mutate_at(vars(INTPTLON, INTPTLAT), as.numeric) %>%
  st_as_sf(coords = c("INTPTLON", "INTPTLAT"),
    agr = "constant",
    crs = 6269,       
    stringsAsFactors = FALSE,
    remove = TRUE) %>%
  st_transform(26918) %>%
  select(STATEFP:AWATER, geometry)

```

```{r, joining, results = FALSE}

# This code does a spatial join between trees and parking data that are within census tracts to the census tract data.

tree_in_tract <- st_join(bal_city_trees_sf, bal_tracts, st_within)

parking_in_tract <- st_join(bal_city_parking_sf, bal_tracts, st_within)


```

```{r, counting, results = FALSE}

# The following counts the number of tree/shrub complaints as well as parking violation complaints for each census tract.

tree_tract_count <- count(as_tibble(tree_in_tract), TRACTCE) %>%
  print()

parking_tract_count <- count(as_tibble(parking_in_tract), TRACTCE) %>%
  print()

```

```{r, graph of the relationship between parking and tree calls}
library(ggplot2)
# Many tree calls may be sign people concerned about look of neighborhood and maybe spend more time outside socializing with neighbors. Would then expect illegal parking to be down because neighbors closer and could mediate parking violations among themselves.

ggplot(tract_tree_parking_sf) + 
  geom_sf(aes(fill = parking_to_tree)) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Parking Violations to\nTree/Shrub Calls")

ggsave("parking_and_tree_calls.jpg")

```

#### An important thing to notice in the following graph is that the ratio is lowest for much of the city, not necessarily because the relationship is true, but because there is probably much less parking going on in these areas of the city where people actually live and thus less parking violation enforcement. Illegal parking enforcement is probably much more intense in downtown where there is a lot of traffic. At the same time, the tree/shrub calls are pretty high near the harbor, no doubt because there is a lot of city property with vegetation here. It is also an area frequented by tourists, so there is more motivation for the city to maintain vegetation here as opposed to other areas.






