## Final Project: Tennessee Ambulance Accessiblity

**Project description:** Mapping accessiblity of ambulantory services to hospitals in the state of Tennessee.


# GES 486, Final Project, Brevin Franklin


```{r, packages, results=FALSE, warning=FALSE, message=FALSE}

library(sf)
library(tidyr)
library(tidycensus)
library(tigris)
library(dplyr)
library(magrittr)
library(ggplot2)
library(measurements)
library(magick)

# census_api_key("b6b09fbf43a886071cd481634f74553533bf5d00", install= TRUE)


```


## Data Management 

```{r, folders, echo=FALSE}

# setwd("~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin")

folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin"

ambulance_data <- "/ambulance_data/final_ambulance_data.csv"

hospital_data <- "/hospital_data/final_hospital_data.csv"

tn_maps_folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin/tn_accessibility_maps"

jack_hos_hex_maps_folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin/jack_hos_accessibility_maps"


```


### Get tigris shapefiles

```{r, getting TN shapefiles, results=FALSE, warning=FALSE, message=FALSE}

# TN STATEFP is 47

# must use projected coordinate system b/c will need distance to find travel-times

# use web mercator to get distances: EPSG:3857

tn_state <- states(cb = TRUE, year = 2020) %>%
  
  filter(STATEFP == "47") %>%
  
  st_transform(crs = "EPSG:3857")

tn_tract <- tracts(state = "TN", cb = TRUE, year = 2020) %>%
  
  st_transform(crs= "EPSG:3857")



```


```{r, reading in ambulance data and convert to sf, results=FALSE, warning=FALSE, message=FALSE}

amb_data <-  read.csv(
  
  paste(folder, ambulance_data, sep = "")
  
  )

hos_data <- read.csv(
  
  paste(folder, hospital_data, sep = "")
  
  )

amb_data_sf <- amb_data %>%
  
  st_as_sf(., coords = c("longitude", "latitude"), crs = "EPSG:4326") %>%
  
   st_transform(crs= "EPSG:3857")
  
  

hos_data_sf <- hos_data %>%
  
  st_as_sf(., coords = c("longitude", "latitude"), crs = "EPSG:4326") %>%
  
   st_transform(crs= "EPSG:3857")



```


```{r, getting only amb_data and hos_data in TN, results=FALSE, warning=FALSE, message=FALSE}

amb_data_tn <- st_join(amb_data_sf, tn_state, join = st_within, left = TRUE) %>%
  
  filter(!is.na(GEOID)) %>%
  
  select(names(.)[c(1:4, 14)])

hos_data_tn <- st_join(hos_data_sf, tn_state, join = st_within, left = TRUE) %>%
  
  filter(!is.na(GEOID)) %>%
  
  select(names(.)[c(1:3, 13)])


```



```{r, accessing mapbox library and api key, eval = FALSE, echo=FALSE}

library(mapboxapi)

mb_access_token("pk.eyJ1IjoiYnJldmluZjEiLCJhIjoiY2t6d3c0NjZ5MDRidzJxbnFncnRpajg5ZyJ9.6IG8RSCpBWvOQgAtDT8deg", install = TRUE, overwrite = TRUE)


```


```{r, getting travel distance matrix, results=FALSE, warning=FALSE, message=FALSE}

# use projection in U.S. feet to get distances in feet and then convert to miles

amb_to_tract_dist <- tn_tract %>%
  st_centroid() %>%
  st_distance(amb_data_tn)

rownames(amb_to_tract_dist) <- tn_tract$GEOID

colnames(amb_to_tract_dist) <- amb_data_tn$name

tract_to_hos_dist <- tn_tract %>%
  st_centroid() %>%
  st_distance(hos_data_tn)

rownames(tract_to_hos_dist) <- tn_tract$GEOID

colnames(tract_to_hos_dist) <- hos_data_tn$name

# get minimum distance for each matrix, then sum the results

amb_to_tract_dist_min <- amb_to_tract_dist %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

# returns distance in kilometers

tract_to_hos_dist_min <- tract_to_hos_dist  %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)


min_dist_for_tracts <- data.frame(tract = tn_tract$GEOID, closest_hos = tract_to_hos_dist_min, closest_amb = amb_to_tract_dist_min)


min_dist_for_tracts$total_dist <- tract_to_hos_dist_min + amb_to_tract_dist_min


tn_tract_and_min_dist <- left_join(tn_tract, min_dist_for_tracts, by = c("GEOID" = "tract"))



```


### Get 2020 5-year ACS data


```{r, calculating hourly wages, results=FALSE, warning=FALSE, message=FALSE}

acs5_2020_var <- load_variables(2020, "acs5", cache =TRUE)

tn_tract_hr_wage <- get_acs(
  
  geography = "tract",
  #Estimate!!Aggregate wage or salary income in the past 12 months (in 2020 inflation-adjusted dollars), ALLOCATION OF WEEKS WORKED IN THE PAST 12 MONTHS FOR THE POPULATION 16 (Estimate!!Total:!!Worked in the past 12 months:), allocation number of hrs worked per week, total number of people worked 12 mnths age 16 to 64 
  variables = c("B19062_001", "B99234_003", "B99233_003", "B99233_002"), 
  
  state = "TN",
  
  year = 2020,
  
  survey = "acs5",
  
  cb = TRUE,
  
  cache_table = TRUE, 
  
  geometry = TRUE,
  
  output = "wide"
  
) %>%
  
  st_transform(st_crs(tn_tract)) %>%
  
  mutate(avr_hr_wage = B19062_001E / (52 * 40 * B99233_002E)) %>%
  
  select(GEOID, avr_hr_wage, geometry)
         
         

#names(tn_tract_hr_wage)[4] <- "est_yr_wage"


```


```{r, making hourly wage adjustments, results=FALSE, warning=FALSE, message=FALSE}

tn_tract_hr_wage <- tn_tract_hr_wage[order(tn_tract_hr_wage$GEOID), ]

tn_tract_and_min_dist <- tn_tract_and_min_dist[order(tn_tract_and_min_dist$GEOID), ]

tn_tract_and_min_dist$hr_wage <- tn_tract_hr_wage$avr_hr_wage

tn_tract_and_min_dist <- tn_tract_and_min_dist %>%
  
  mutate(wage_adj_hos_dist = closest_hos / hr_wage) %>%
  
  mutate(wage_adj_amb_dist = closest_amb / hr_wage) %>%
  
  mutate(wage_adj_tot_dist = total_dist / hr_wage)


```


## TN State Maps


```{r, mapping Nearest Ambulatory Care to Census Tract}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

amb_care_map <- ggplot(tn_tract_and_min_dist, aes(fill = closest_amb)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Distance (Kilometers)",
       title = "Estimated Travel Distance 
(Nearest Ambulatory Care to Census Tract)",
       subtitle = "Census Tracts in Tennessee")

amb_care_map



```




```{r, mapping Nearest Ambulatory Care to Census Tract, hr wage adjusted}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

amb_care_wage_adj_map <- ggplot(tn_tract_and_min_dist, aes(fill = wage_adj_amb_dist)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Wage Weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Ambulatory Care to Census Tract)",
       subtitle = "Census Tracts in Tennessee")

amb_care_wage_adj_map


```


```{r, mapping Nearest Nearest Hospital to Census Tract}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

hos_care_map <- ggplot(tn_tract_and_min_dist, aes(fill = closest_hos)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Distance (Kilometers)",
       title = "Estimated Travel Distance 
(Nearest Hospital to Census Tract)",
       subtitle = "Census Tracts in Tennessee")

# notice scale changes significantly

# accounting for scale change seems like more areas are in close proximity to hospitals than ambulatory relative to scale change

hos_care_map

```


```{r, mapping Nearest Nearest Hospital to Census Tract, wage adjusted}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

hos_care_wage_adj_map <- ggplot(tn_tract_and_min_dist, aes(fill = wage_adj_hos_dist)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Wage Weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Hospital to Census Tract)",
       subtitle = "Census Tracts in Tennessee")

# notice scale changes significantly

# accounting for scale change seems like more areas are in close proximity to hospitals than ambulatory relative to scale change

hos_care_wage_adj_map

```



```{r, mapping Nearest Ambulatory Care to Census Tract to Nearest Hospital}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

tot_care_map <- ggplot(tn_tract_and_min_dist, aes(fill = total_dist)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Distance (Kilometers)",
       title = "Estimated Travel Distance 
(Nearest Ambulatory Care to Census Tract to Nearest Hospital)",
       subtitle = "Census Tracts in Tennessee")

tot_care_map

```



```{r, error=FALSE, warning=FALSE, message=FALSE}

# Note the three areas of poor service seem to be areas that are closer to a hospital/ambulatory care unit outside of state (you removed the sites not in Tennessee state)

tot_care_wage_adj_map <- ggplot(tn_tract_and_min_dist, aes(fill = wage_adj_tot_dist)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) + 
  theme_void() + 
  labs(fill = "Wage Weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Ambulatory Care to Census Tract to Nearest Hospital)",
       subtitle = "Census Tracts in Tennessee")

tot_care_wage_adj_map

```

```{r, saving above tennessee map images, eval = FALSE, echo=FALSE}

tn_state_maps <- c("hos_care_map", "hos_care_wage_adj_map", "hos_care_map", "hos_care_wage_adj_map", "tot_care_map", "tot_care_wage_adj_map")

count <- 0

tn_map_names <- c()

for (file in tn_state_maps) {
  
  count <- count + 1

  
  map_object <- get(file)
  
  new_name <- paste("tn_map_", count, ".jpeg", sep = "")
  
  tn_map_names[count] <- new_name
  
  assign(new_name, map_object)
  
  ggsave(new_name, device = "jpeg", path = tn_maps_folder)
  
  rm(new_name)
  
}




```


```{r, writing animation for Tennessee maps, eval = FALSE, echo=FALSE}

image_names <- tn_map_names

map_folder <- tn_maps_folder

images <- c(
  
  image_read(
    
    paste(map_folder, "/", image_names[1], sep = "")
  ),
  
  image_read(
  
    paste(map_folder, "/", image_names[2], sep = "")
  ),
  
  image_read(
    
    paste(map_folder, "/", image_names[3], sep = "")
  ),
  
  image_read(
    
    paste(map_folder, "/", image_names[4], sep = "")
  ),
  
  image_read(
    
    paste(map_folder, "/", image_names[5], sep = "")
  ),
  
  image_read(
    
    paste(map_folder, "/", image_names[6], sep = "")
  )
  
)

image_append(image_scale(images, "x400"))

my.animation <-image_animate(image_scale(images, "800x800"), fps = 0.5, dispose = "previous")

image_write(my.animation, 
            path = paste(map_folder, "/tn_maps_animation.gif", sep = "")
)
  
# if there is a function to assign an object to a string, is there a function to reference an object by a string

# use the get() function to do the above, it gets an object reference by a string representing its name
  

### dont see a very clear pattern between elderly population and temperature, at least using data from the count level. May be good idea to use spatial regression in the future
#### or figure out to get elderly pop. at a level not determined by population or even a geography that is determined randomly (ex. census blocks)

### figure out how to make state borders thicker

my.animation



```





## Hex maps for Jackson Hospital Service Shed

```{r setup, warning=FALSE, message=FALSE, results=FALSE}
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(areal)

library(curl)
library(jsonlite)
library(foreach)
library(readr)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#census_api_key("YOURKEYHERE")

```



### Jackson Hospital Isochrone


```{r, Jackson Hospital and Coords, warning=FALSE, message=FALSE, results=FALSE}

jack_hos <- as.data.frame(hos_data[hos_data$hosp_id == 58, ])

jack_hos$WP <- paste(jack_hos$latitude, jack_hos$longitude, sep=",")

jack_hos_sf <- hos_data_tn[hos_data_tn$hosp_id == 58, ]


```




### Make Isochrone Function

Dillon's Code

This sections builds on a couple of different resources to build the `bing_write_isochrones` which we'll use to save query bing maps and write the isochrones to disk.

Relevant reading:
- Scraping with `curl`: https://cran.r-project.org/web/packages/curl/vignettes/intro.html
- Calculate an isochrone with Bing Maps: https://docs.microsoft.com/en-us/bingmaps/rest-services/routes/calculate-an-isochrone
```{r test_call, eval = FALSE}
x.result <- NULL
x.resources <- NULL
bing_write_isochrones <- function(latlon, name, dateTime, dateName, travelMode, maxTime) {
  # NOT RUN / DEBUG
  if(FALSE) {
    latlon = iter.loc$WP
    name = iter.loc$Name
    dateTime = "2022-03-16T15:00:00-05:00"
    dateName= "Tue1500"
    maxTime = iter.pds
    travelMode = iter.mode
  }
  
  ## make the call
  req <- paste0("http://dev.virtualearth.net/REST/v1/Routes/IsochronesAsync",
                "?wp=", latlon,
                "&maxTime=", maxTime,
                "&timeUnit=minute",
                "&dateTime=2020-01-10T18:00:00-08:00",
                "&travelMode=", travelMode,
                "&key=ArXMzkC9M_KiC3yUAXKuqvZ9vLuycMpX3sL7RI3gGAPIqDoyHzGdimkIy33fOwk4")
  
  # see if can change bing api key above
  
  print(paste("1. Fetching data with URL:", req))
  Sys.sleep(3)
  req.request <- curl_fetch_memory(req)
  req.json <- fromJSON(rawToChar(req.request$content))
  req.reqdf <- as.data.frame(req.json$resourceSets$resources)
  req.callbackURL <- req.reqdf$callbackUrl
  
  ## make the callback
  Sys.sleep(3)
  print(paste("2. Fetching callback with URL:", req.callbackURL))
  req.callback <- curl_fetch_memory(req.callbackURL)
  req.callbackjson <- fromJSON(rawToChar(req.callback$content))
  req.callbackdf <- as.data.frame(req.callbackjson$resourceSets$resources) 
  req.resultURL <- req.callbackdf$resultUrl
  
  ## get the result, but pause a second for it to compute
  Sys.sleep(4)
  x.result <<- req.callbackjson
  print(paste("3. Fetching result with URL:", req.resultURL))
  req.result <- curl_fetch_memory(req.resultURL)
  req.resultjson <- fromJSON(rawToChar(req.result$content))
  req.resultpolys <- as.tibble(as.data.frame(req.resultjson$resourceSets$resources))
  x.resources <<- req.resultpolys
  dput(req.resultpolys,"debug_dput.txt")
  
  ## unpack and repackage lat/lon from result
  req.multicoords <- req.resultpolys$polygons[[1]]$coordinates
  
  ret.sf <- NULL
  if(length(req.multicoords) > 0) {
    print(paste("4. Found",length(req.multicoords),"polygons"))
    print(paste("5. Merging",length(req.multicoords),"polygons:"))
    foreach(i = 1:length(req.multicoords)) %do% {
      ## get coordinates for the i_th polygon
      singlecoords <- unlist(req.multicoords[i])
      lon <- singlecoords[(length(singlecoords)/2+1):(length(singlecoords))]
      lat <- singlecoords[1:(length(singlecoords)/2)]
      coordinates <- tibble(lat, lon)
      
      ## create sf spatial polygons
      single.poly <- coordinates %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      
      if(is_empty(ret.sf)) {
        ret.sf <- single.poly
        print(paste(" | Assigning",i,"th polygon to ret.sf"))
      } else {
        ret.sf <- st_union(ret.sf, single.poly)  
        print(paste(" | Unioning",i,"th polygon to ret.sf"))
      }
    }
    print(paste("6. Completed polygon merge."))

    ret.sf$travelTime = maxTime
    ret.sf$travelMode = travelMode
    
    timestring <- str_replace_all(str_replace_all(Sys.time()," ", "_"),":","-")
    
    # (latlon, name, dateTime, dateName, travelMode, maxTime)
    ret.filename <- paste(name,dateName,travelMode,maxTime,timestring,".geojson", sep="_")
    st_write(ret.sf, ret.filename, driver="GeoJSON")
    print(paste("F. Wrote to file:",ret.filename))
    print(paste("———————————————————————————————."))
  }
  
  return(TRUE)
}
```



## Getting Driving Isochrones for Jackson Hospital Service Shed
```{r driving_combos, eval = FALSE}

setwd("~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin/jack_hos_isochrome")

dir_for_isochrome <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin/jack_hos_isochrome"

# can change walking to "transit" and time to different time to reflect normal traffic conditions

# for each ir (the index name) in nrow(sl), basically saying for each row in sl

# isochrome polygon is made for each Clinc in the orriginal data meeting the above criteria

bing_write_isochrones(latlon = jack_hos[1,]$WP, name = jack_hos[1,]$jfdm_id, maxTime = 15, travelMode = "driving", 
                      # or "walking"
                      dateTime = "2022-05-9T15:00:00-05:00", dateName= "Tue1500")
Sys.sleep(1)


```


### Reading in isochrone

```{r, looking at isochrome, warning=FALSE, message=FALSE, results=FALSE}

dir_for_isochrome <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Final_Project_Brevin_Franklin/jack_hos_isochrome"


# looking at 15 shed with hospital and ambulances means at max, takes 30 minutes for amb to pick somone up and take them to hospital

jack_hos_isochrome <- st_read(paste(dir_for_isochrome, "/_Tue1500_driving_15_2022-05-17_16-18-10_.geojson", sep ="")) %>%
  
  st_as_sf() %>%
  
  st_transform("EPSG:3857")

# transform to correct projected coordinate system

plot(jack_hos_isochrome$geometry)

plot(jack_hos_sf$geometry, add = TRUE)

```


### Find all census tracts in in isochrone

```{r, warning=FALSE, message=FALSE, results=FALSE}


tn_tract_in_jack_hos_shed_indicies <- unlist(
  
  st_contains(jack_hos_isochrome, 
              
              st_centroid(tn_tract_and_min_dist)
              
  )
  
)


tn_tract_in_jack_hos_shed <- tn_tract_and_min_dist[tn_tract_in_jack_hos_shed_indicies, ]

plot(jack_hos_isochrome$geometry)

plot(tn_tract_in_jack_hos_shed$geometry, add = TRUE)


```


### Find all ambulatory service units in isochrone

```{r, find all amb inside service shed, results = FALSE, warning=FALSE, message=FALSE}

amb_in_jack_hos_shed_indicies <-  unlist(
  
  st_contains(
    
    jack_hos_isochrome,
    
    amb_data_tn
              
  )
  
)


amb_in_jack_hos_shed <- amb_data_tn[amb_in_jack_hos_shed_indicies, ]

plot(jack_hos_isochrome$geometry)

plot(amb_in_jack_hos_shed$geometry, add = TRUE)


```


### Make and subset grid

(Dillon's Code)

```{r makegrid, results=FALSE}
# Make a grid
tn_tract_in_jack_hos_shed_grid <- st_make_grid(tn_tract_in_jack_hos_shed,
  2 * 1000, # Kms
  crs = "EPSG:3857",
  what = "polygons",
  square = FALSE # hexagons
)

# To sf
tn_tract_in_jack_hos_shed_grid <- st_sf(index = 1:length(lengths(tn_tract_in_jack_hos_shed_grid)), tn_tract_in_jack_hos_shed_grid) # Add index

#plot(st_geometry(bmore_grid), border="#aaaaaa", lwd = .1)
#plot(st_geometry(bmore_bg_income), add=T, lwd = .1)

tn_tract_in_jack_hos_shed_shape <- st_union(tn_tract_in_jack_hos_shed)

tn_tract_in_jack_hos_shed.intersects <- st_intersects(tn_tract_in_jack_hos_shed_shape, tn_tract_in_jack_hos_shed_grid)
tn_tract_in_jack_hos_shed.subset <- tn_tract_in_jack_hos_shed_grid[tn_tract_in_jack_hos_shed.intersects[[1]],]


plot(st_geometry(tn_tract_in_jack_hos_shed.subset), col="red")

```


### Areal Interpolation

Make sure to install `areal` package.

(Dillon's Code)

```{r, weights for total distance, results = FALSE}

# Validate areal weighted extraction
ar_validate(source = tn_tract_in_jack_hos_shed, target = tn_tract_in_jack_hos_shed.subset, varList = "wage_adj_hos_dist", method = "aw")


tn_tract_in_jack_hos_shed_grid_dist <- aw_interpolate(tn_tract_in_jack_hos_shed.subset, tid = index, source = tn_tract_in_jack_hos_shed, sid = "GEOID",
               weight = "sum", output = "sf", extensive = c("wage_adj_hos_dist", "wage_adj_amb_dist",
                                                            "wage_adj_tot_dist"))

#plot(st_geometry(bmore_grid_income))
```


## Repeating the previous analysis but with Maps of the Jackson Hospital Service Shed

```{r, Jack Hos nearest amb, wage adjusted}

# Get an idea if the "closeness" of the distance measure and the time measure seem to find the same thing

jack_map1 <- ggplot() +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid,
    fill = "white", colour = "gray80"
  ) +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid_dist,
    mapping = aes(fill = wage_adj_amb_dist), show.legend = FALSE
  ) +
  labs(fill = "Wage weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Ambulatory Care to Census Tract 
within 15 minute Isochrome)") +
  coord_sf()

jack_map1

# parts to east are with lower accessiblity
```

```{r, Jack Hos nearest hos, wage adjusted}

# Get an idea if the "closeness" of the distance measure and the time measure seem to find the same thing

jack_map2 <- ggplot() +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid,
    fill = "white", colour = "gray80"
  ) +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid_dist,
    mapping = aes(fill = wage_adj_hos_dist), show.legend = FALSE
  ) +
  labs(fill = "Wage weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Hospital Care to Census Tract 
within 15 Minute Isochrome)") +
  coord_sf()

jack_map2
# parts to east are with lower accessiblity
```



```{r, Jack Hos nearest amb to tract to nearest host, wage adjusted}

# Get an idea if the "closeness" of the distance measure and the time measure seem to find the same thing

jack_map3 <- ggplot() +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid,
    fill = "white", colour = "gray80"
  ) +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid_dist,
    mapping = aes(fill = wage_adj_tot_dist), show.legend = FALSE
  ) +
  labs(fill = "Wage weighted Distance",
       title = "Estimated Travel Distance 
(Nearest Hospital Care to Census Tract 
within 15 Minute Isochrome)") +
  coord_sf()

jack_map3

# parts to east are with lower accessiblity
```



```{r, saving above tn map images, eval=FALSE, results=FALSE, echo=FALSE}

jack_hos_maps <- c("jack_map1", "jack_map2", "jack_map3")

count <- 0

jack_map_names <- c()

for (file in jack_hos_maps) {
  
  count <- count + 1

  
  map_object <- get(file)
  
  new_name <- paste("jack_map_", count, ".jpeg", sep = "")
  
  jack_map_names[count] <- new_name
  
  assign(new_name, map_object)
  
  ggsave(new_name, device = "jpeg", path = jack_hos_hex_maps_folder)
  
  rm(new_name)
  
}




```




```{r, writing animation for TN maps, eval=FALSE, echo=FALSE}

image_names <- jack_map_names

map_folder <- jack_hos_hex_maps_folder

images <- c(
  
  image_read(
    
    paste(map_folder, "/", image_names[1], sep = "")
  ),
  
  image_read(
  
    paste(map_folder, "/", image_names[2], sep = "")
  ),
  
  image_read(
    
    paste(map_folder, "/", image_names[3], sep = "")
  )
  
)

image_append(image_scale(images, "x400"))

my.animation <-image_animate(image_scale(images, "800x800"), fps = 0.5, dispose = "previous")

image_write(my.animation, 
            path = paste(map_folder, "/jack_maps_animation.gif", sep = "")
)
  
# if there is a function to assign an object to a string, is there a function to reference an object by a string

# use the get() function to do the above, it gets an object reference by a string representing its name
  

### dont see a very clear pattern between elderly population and temperature, at least using data from the count level. May be good idea to use spatial regression in the future
#### or figure out to get elderly pop. at a level not determined by population or even a geography that is determined randomly (ex. census blocks)

### figure out how to make state borders thicker

my.animation



```




```{r, eval=FALSE, results=FALSE, echo=FALSE}

## Don't use the following

# Uses bmore_shape, see above
tn_tract_in_jack_hos_shed.crop <- st_intersection(tn_tract_in_jack_hos_shed_grid, tn_tract_in_jack_hos_shed)

#plot(st_geometry(bmore_shape))
#plot(st_geometry(bmore_grid_income.crop))

ggplot() +
  geom_sf(
    data = tn_tract_in_jack_hos_shed_grid,
    fill = NA, colour = "gray80"
  ) +
  geom_sf(
    data = tn_tract_in_jack_hos_shed.crop,
    mapping = aes(fill = wage_adj_amb_dist), show.legend = TRUE
  ) +
  geom_sf(
    data = jack_hos_isochrome,
    fill = NA, colour = alpha("gray80",0.2)
  ) +
  coord_sf() + theme_minimal()

```




