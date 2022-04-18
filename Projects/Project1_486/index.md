## Project 1: Temperature and the Elderly

**Project description:** 
This project will attempt to map how temperature and the elderly population are distributed across the United States. This will be done using isotherms to represent areas of similar temperature and dots to symbolize density of elderly population.

#                               Project 1, GES 486, Brevin Franklin


## Research Question:

###       What is the relationship between dense elderly populations and temperature in the continental United States? There is a cliché in the United States that when people retire, they move to Florida to live out their golden years. Even for those that do not leave behind their old place of residence, it is common for elderly so-called “Snowbirds” to spend winter in the sunbelt. This cliché if true could have serious policy implications for states with larger elderly populations. For example, elderly people are much more likely to vote than other groups, due to their more abundant leisure time. This can change the political agenda in a place, especially if the old-aged prefer policies that benefit them at the expense of others. Referendums in favor of public-school renovations may perform poorly with the elderly, but others in favor of public parks and space may perform better. In the case of the “Snowbirds,” thousands more winter residents who use public resources, but fail to live in a state long enough to pay income taxes may strain public coffers or expand them if sales revenue increases. Separate from political and economic concerns, the elderly people vulnerable to temperature, whether it be extreme cold or extreme heat. Knowing where elderly population and extreme weather co-occur may give better insights into what regions have the most vulnerable (e.g., Florida may be chief among them). In any case, the relationship between the old-aged and climate may be important in the future to come.




### Extra information helpful for working with NOAA Data, and unloading the files through windows powershell.

#### NOAA website for climate station data:

##### "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/"

#### The file ghcnd_hcn.tar.gz contains U.S. historical weather station data.

##### Code to unzip gz.tar files in windows powershell using 7-zip

##### 7z x filename.tar.gz
##### 7z x filename.tar

#### Windows Powershell code for separating the US station data csv files from global station data csv files


##### Get-ChildItem C:\Source -recurse -filter '*US*.csv' | Copy-Item -destination C:\Destination

###### The above code works best if destination folder is not a subfolder of the source

##### Use this code to change file type tag to one that is easier to work with

###### Get-ChildItem -Recurse -Include *.tavg | Rename-Item -NewName { $_.Name.replace(".tavg",".txt") }

#### Once all U.S. files in new directory will need loops to do data merging


```{r, To get final workspace Data Frames to run code without recreating data frames with code, run following, results = FALSE, echo=FALSE, warning=FALSE}

load("~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Project1_Brevin_Franklin/2022_03_31_0446pm_Project1_Brevin_Franklin.RData")

```

### Getting Weather Station from Files Stored in Folder on Disk

```{r get folder data, eval = FALSE, echo = FALSE}

folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Project1_Brevin_Franklin"

stations <- list.files(
  paste(folder, "/ghcnd_hcn", sep = "")
  )

  
library(dplyr)

library(tidyr)

# following library contains the function celsius.to.fahrenheit for temperature conversions

library(weathermetrics)

library(stringr)

library(udunits2)


```



```{r, making data frame for coordinates, eval = FALSE, echo=FALSE}

station_info <- read.fwf(
      
      paste(
        
        cbind(folder, "/", "ghcnd-stations.txt"), collapse = ""),
      
      header = FALSE,
      
      width = 86
      
      # width here is width of the text file
)%>%
  
  separate(V1, 
            c("station", "latitude", "longitude", "elevation", "state", "name", "gsn", "hcn", "wmo_id"),
            sep = c(11,20,30,37,40,71,75,79)
   )

station_info$station2 <- station_info$station

station_info <- as.data.frame(
  
  lapply(station_info, trimws) 
  ) %>%
  
  separate(station2, c("station_country", "station_other"), sep = 2) %>%
  
  filter(station_country == "US")

station_info[ ,c("station_country", "station_other")]  <- NULL

station_info[ , c("latitude", "longitude", "elevation")] <- lapply(station_info[ , c("latitude", "longitude", "elevation")], as.numeric)

station_info$elevation[station_info$elevation == -999.9] <- NA

station_info[ , c("elevation")] <- ud.convert(station_info[ , c("elevation")], "meters", "feet")


```


```{r, merging stations in folder will all their info, eval = FALSE, echo = FALSE}

station <- gsub(".txt", "", stations)

## use the following to check for duplicates:                                       anyDuplicated(station_names), anyDuplicated(station_info) 

station <- as.data.frame(station)

station_loc_and_med_temp <- inner_join(station, station_info, by = "station")

rm(station)

```

## Combining Weather Station Data from files on Disk into a Single Data Frame with Minimum and Maximum Daily Temperature. This is done for the First 28 Days of Each Month in Every Year for which a Station has Observations.

```{r, data management, results = FALSE, eval = FALSE, warning=FALSE, message=FALSE}

for (yr in 2008:2018) {
  
  station_temp_stats <- data.frame(station = c(""), median = c(0), variance = c(0)) 
  
  for (i in 1:length(stations)) {
    
    station <- read.delim2(
      
      paste(
        
        cbind(folder, "/ghcnd_hcn/", stations[i]),
        
        collapse = ""),
      
      sep = cat(" ", " "), header = FALSE)
    
    station2 <- station %>%
    separate(V1, c("name", "date"), sep = 11) %>%
    separate(date, c("year", "month"), sep = 4) %>%
    separate(month, c("month", "type"), sep = 2) %>%
    separate(type, c("type", "missing"), sep = 4) %>% 
    filter(
      (type =='TMAX' | type == 'TMIN') & missing != "-9999")
   
    station2 <- station2 %>% 
      select(
        c("name", "year", "month", "type"), 
        num_range("V", seq(from = 2, to = length(station2), by = 2)
                  )
        )
    
    station2$year <- as.numeric(station2$year)
    
    station2$month <- as.numeric(station2$month)
    
    station2 <- station2 %>% filter(year <= 2008)
    
  
  # later on, use filter to subset the years want to look at
  
    station_temp_stats[i,1] <- station2[1,"name"]
    
    # be careful to count correct length:                                                                          4 (number of non temp columns) + x (number of temp cols.) = length(station2)
    
    names(station2)[5:length(station2)] <- as.character(1:(length(station2) - 4))
    
    station2[ ,5:length(station2)] <- lapply(station2[ ,5:length(station2)], as.numeric)
    
    station2[station2 == -9999] <- NA
    
    station2[ ,5:length(station2)] <- celsius.to.fahrenheit(
      (1 / 10) * station2[ ,5:length(station2)]
      )
    
    station3 <- data.frame(temp=unlist(station2[ ,5:length(station2)]))
    
    # can use either of following to look at distribution: hist(test3$temp), hist(test3[ ,1])
    
    # need the na.rm to ignore the NAs in the data
    
    station_temp_stats[i,2] <- median(station3[ ,1], na.rm = TRUE)
    
    station_temp_stats[i,3] <- var(station3[ ,1], na.rm = TRUE)
    
    #### figure out how to loop through all files in station data folder
    
    ## figure out how to remove rows that are empty, have missing values, or contain characters
    ## also figure out how to convert character columns to numeric in data frame
    
    # use print statement to check: print(stations[i]," ", i, " of 1218")
    
    rm(station, station2, station3)
    
    print(paste(i, " of ", length(stations), " for ", yr))
    
  }
  
  names(station_temp_stats)[2:3] <- paste(names(station_temp_stats)[2:3], "_", yr, sep = "")
  
 
  
  # to assign string to some object can use following code:  title <-           paste0("station_temp_stats_", yr) then assign(title, station_temp_stats)
  
  station_loc_and_med_temp <- inner_join(station_loc_and_med_temp, station_temp_stats, by = "station")
  
  rm(station_temp_stats)


}

# Whole process takes about 45 minutes to an hour!

# how do you reference a data frame just by the string that represents its name so can loop through the different data frames
  # is there a function in R that takes a string and returns the dataframe with the matching string as its name?

```

```{r, importing libraries, echo = FALSE, results= FALSE, warning=FALSE}

library(data.table)

library(tigris)

library(sf)

library(ggplot2)

library(plotly)

library(reshape2)

library(stringr)

library(purrr)

library(crsuggest)

library(tidycensus)

library(extrafont)

## Install package

library(devtools)

#devtools::install_github("vladmedenica/themedubois")

#dubois ggplot theme

library(themedubois)

library(tidytuesdayR)

library(tidyverse)

library(tmap)

library(akima)

#library(gsheet) #Links to the data in a Google Sheet

#library(GGally) #Extension of ggplot mapping attributes

#library(rgdal) #Provides geospatial attributes

#library(usmap) #Provides a basemap of the US

library(magick) #Processes images

```

```{r, exporting station data file as csv, echo= FALSE, eval = FALSE}


new_folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Project1_Brevin_Franklin/2008_2018_station_median_temps"

dir.create(new_folder)

fwrite(station_loc_and_med_temp, 
       paste(folder, "/station_loc_and_med_temp.csv", collapse = "", sep = "")
)

# removing the dataframes and values no longer needed
drop_files <- 
rm(list = ls()[c(1:4, 6:19)])

```


## Getting Shapefiles for the Entire United States from the Census Bureau

```{r, get map of census tracts in the U.S., eval = FALSE}

# use block groups, b/c not determined by pop, also use ACS5 estimates b/c ACS1 data not avaiable for that level 

options(tigris_use_cache = TRUE)

rappdirs::user_cache_dir("tigris")

### STATEFP is a character vector

us_counties <- counties(cb = TRUE, year = 2015) %>% 
  filter(STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78" & STATEFP != "15" & STATEFP != "02")

us_states <- states(cb = TRUE, resolution = "20m") %>% 
   filter(STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78" & STATEFP != "15" & STATEFP != "02")


```

## Transforming Shapefiles to a Better Projection

```{r, transform shapefiles, eval = FALSE}

# Converting to Lambert Conformal Conic with datum NAD83

us_counties_projected <- st_transform(us_counties, crs = "ESRI:102004")

us_states_projected <- st_transform(us_states, crs = "ESRI:102004")

#ggplot(us_states_projected) + 
  #geom_sf() + 
  #theme_void()


```

## Converting Station Data with Longitude and Latitude Columns to sf Object

```{r, convert temp data to sf, eval = FALSE}

station_loc_and_med_temp_sf <- st_as_sf(station_loc_and_med_temp, 
                                        coords = c("longitude","latitude"),
                                        crs = 4326
                                        ) %>% 
  st_transform("ESRI:102004")

station_loc_and_med_temp_sf[ , c("longitude", "latitude")] <- st_coordinates(station_loc_and_med_temp_sf)

### need to convert to dataframe to use contour plot code; that code needs the columns to be in numeric form, not double

station_loc_and_med_temp <- as.data.frame(st_drop_geometry(station_loc_and_med_temp_sf))


```

## Calculating Median Temperature from Earliest Recordings to Each Year in the Series 2014 to 2018

```{r, getting a data frame for each year station median temperature, eval = FALSE}

median_yr <- paste("median_", 2014:2018, sep = "")

temp_df_names <- c("")

COUNT <- 1

for (element in median_yr) {


  df <- station_loc_and_med_temp[ , c("longitude", "latitude", element)]
  
  df[ , c(element)] <- round(df[ ,c(element)], digits = 0)
  
  
  #dev.off()
  
  df2<- with(df, interp(x=longitude, y=latitude, z= df[ , c(element)]))
  
  # the variables in the data frame now need to be referenced as x,y, and z not original names
  
  df3 <- as.data.frame(interp2xyz(df2))
  
  name <- paste(element, "_df", sep = "")
  
  temp_df_names[COUNT] <- name
  
  assign(name, df3)
  
  rm(df, df2, df3)
  
  COUNT <- COUNT + 1


}


#ggplot(df3, aes(x = x, y = y, z= z)) +
  #geom_contour_filled() +
  #geom_contour() 



```


## Getting 5-year ACS Data for County-Level Elderly Population in Each State (2015 to 2019)

```{r, geting ACS data for the elderly population by census tract from 2015 to 2019, eval = FALSE}


# want to make dot density map 

# how do you get around the fact that get_acs won't retrieve census tract and block data for the entire country?

# From Walker With respect to geography and the American Community Survey, users should be aware that whereas the 5-year ACS covers geographies down to the block group, the 1-year ACS only returns data for geographies of population 65,000 and greater. This means that some geographies (e.g. Census tracts) will never be available in the 1-year ACS, and that other geographies such as counties are only partially available. 

# lowest geographic level can get with get_acs for entire US is county (ACS5 gives best results)

elderly_pop_file_names <- c("")

for (i in 2015:2019) {
  elderly_pop <- get_acs(
    geography = "county",
    year = i,
    variables = "B16004_046",
    geometry = TRUE,
    survey = "acs5"
  ) %>% 
    separate(GEOID, c("STATEFP", "other"), sep = 2) %>%
    
     filter(STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78" & STATEFP != "15" & STATEFP != "02") %>% 
    st_transform("ESRI:102004")
  
  title <- paste0("elderly_pop_", i)
  
  elderly_pop_file_names[i - 2014] <- title
  
  assign(title, elderly_pop)
  
  rm(elderly_pop, title)
}

## figure out way to get dot density version of this data to fit with map of US states with the position shifted
```

## Mapping Elderly Population Dots to Represent Density

### Each dot represents 10,000 people

```{r, making dot density maps, eval=FALSE}

elderly_pop_dot_file_names <- c("")

COUNT = 2015

for (item in elderly_pop_file_names) {
  
  file <- get(item)

  elderly_pop_dots <- file %>%
      mutate(est10000 = as.integer(estimate / 10000)) %>%
      st_sample(size = .$est10000, exact = FALSE) %>%
      st_sf() %>%
    slice_sample(prop = 1)
  
  elderly_pop_dots[ , c("longitude", "latitude")] <- st_coordinates(elderly_pop_dots)
  
  title <- paste("elderly_dots", COUNT, sep = "")
  
  elderly_pop_dot_file_names[COUNT - 2014] <- title 
    
  assign(title, elderly_pop_dots)
  
  rm(elderly_pop_dots, title)
  
  COUNT <- COUNT + 1


}

# how to get this to project correctly with map you are using

# have to figure out how to make gifs

```


## Mapping Code

### The following code block creates maps displaying the previously created dots representing density, the United States shapefile, and temperature contours 


####        The following	maps contain a dot density file overlayed onto isotherms (temperature contour lines) for the entire United States. I retrieved the elderly population data from the 5-year ACS for the year specified. The temperature data are lagged by one year.
            The general method of creating these maps was to pull from a file of historical weather station data to create the isotherms. Create dots from the 5-year ACS using the associated geometry. This was all will the goal of showing how well density and certain temperatures co-occur.
            In the past weeks, I have created a workflow that wrangles data from NOAA. This data is from weather stations in the United States with some observations extending back a century. This data, which contains latitude and longitude of each station, are used to create isotherms. In addition, I have written code that gets data from the Census Bureau and from it, creates dot density points from it. Combined with code that gets a map of the United States from the Census Bureau shapefiles, this all creates a map. Loops for each year under study create different maps for different years. Code that creates a gif then strings them into a moving image. Finally, with 1-year ACS data for elderly migration, a graph in the style of W.E.B. Dubois’ plots migration for seniors in different age groups.

```{r, working map with contours, dots, and U.S. states, results = FALSE, warning=FALSE, message=FALSE}

map_folder <- "~/Old_Desktop_Files_2016-2021/8_Semester_UMBC/GES_486_Advanced_Application_in_GIS/Project1_Brevin_Franklin/maps"

#dir.create(map_folder)

# <- paste("elderly_pop_", 2015:2019, "_dots", sep = "")

## using us_states_projected$x automatically references what you have set to longitude in the geometry in us_states_projected even though there is not a column specifically called x

# change elderly_pop_files_names to the list fro elderly_dot file names
temp_and_dots <- cbind.data.frame(temp_df_names, elderly_pop_dot_file_names)

# vector containg images made by loop

image_names <- c()

for (j in 1:5) {
  
  dots_df <- get(temp_and_dots[j, c("elderly_pop_dot_file_names")])
  
  temp_df <- get(temp_and_dots[j, c("temp_df_names")])

  ggplot(us_states_projected, aes(us_states_projected$x, us_states_projected$y)) + 
    
    geom_contour_filled(temp_df, mapping = aes(x = temp_df$x, y = temp_df$y, z= temp_df$z)) +
    
    geom_contour(temp_df, mapping = aes(x = temp_df$x, y = temp_df$y, z= temp_df$z)) +
    
    geom_sf(fill = "transparent") +
    
    geom_point(data = dots_df, mapping = aes(
      x = dots_df$longitude, y = dots_df$latitude), colour = "red", size = 0.001
      ) +
    labs(
      
      x = NULL,
      
      y = NULL,
      
      title = paste("\t\t\t\tDensity of Elderly Population, ", 2014 + j, ", with Isotherms", sep = ""),
      
      #paste() won't recognize newline characters, only gives character vector; cat will recognize newline characters and will return a string; caption likes character vectors and won't print output from paste()
      
      caption = paste("*Each dot represents 10,000 people\nData sources:\n", 2014 + j, "5-YEAR ACS, US CENSUS BUREAU\nDAILY GLOBAL HISTORICAL CLIMATOLOGY NETWORK (GHCN-DAILY) NOAA"),
      
       fill = paste("Median Temp (\U00B0 F)", 2013 + j, sep = " ") 
      ) +
    # using theme_void() will mess of text spacing, should use theme() and following code instead
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()
)
  
  filename <- paste("map_test_", j + 2014, ".jpeg", sep = "")
  
  ggsave(filename, device = "jpeg", path = map_folder)
  
  image_names[j] <- filename

  rm(dots_df, temp_df, filename)

  
}

## figure out how to do the following with vectors or with loops
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
  )
  
)

image_append(image_scale(images, "x400"))

my.animation <-image_animate(image_scale(images, "800x800"), fps = 0.5, dispose = "previous")

image_write(my.animation, 
            path = paste(map_folder, "/maps_project1.gif", sep = "")
)
  
# if there is a function to assign an object to a string, is there a function to reference an object by a string

# use the get() function to do the above, it gets an object reference by a string representing its name
  

### dont see a very clear pattern between elderly population and temperature, at least using data from the count level. May be good idea to use spatial regression in the future
#### or figure out to get elderly pop. at a level not determined by population or even a geography that is determined randomly (ex. census blocks)

### figure out how to make state borders thicker

my.animation

```


### Gif of Maps Made by the Previous Code

```{r, maps gif, warning=FALSE}

my.animation

```


```{r, ACS variable options, eval = FALSE, echo= FALSE}

acs1_vars <-load_variables(2015, "acs1")

acs5_vars <-load_variables(2015, "acs5")

View(acs5_vars)

# see that variable code B16004_046 from the table on languages spoken at home is a consist variable measuring the estimated total of those 65 years and older 



```


```{r, walkers way of doing dot density, echo = FALSE, eval = FALSE}

# should alter size of dots or how many people each dot represents
tm_shape(us_states_outside, 
         projection = sf::st_crs("ESRI:102009")) + 
  tm_polygons(col = "white", 
              border.col = "grey") + 
  tm_shape(elderly_pop_dots_2019) +
  tm_dots(col = "red", 
          palette = "Set1",
          size = 0.001, 
          title = "elderly population")

```


## Getting 1-year ACS data for Elderly Within State, County to County Migration (2006 to 2019)

```{r, get acs data for elderly migration, results= FALSE, warning=FALSE, message=FALSE}

st_mig_county_to_county <- data.frame(
  GEOID = c(""), NAME = c(""), variable = c(0), estimate = c(0), moe = c(0), year = c(0)
  )

for (i in 2006:2019) {
  
  elderly_mig <- get_acs(
    geography = "us",
    year = i,
    variables = c(age_65_69 = "B07001_062", age_70_74 = "B07001_063", age_75_up = "B07001_064"),
    geometry = FALSE,
    survey = "acs1") 
  
  elderly_mig$year <- i
  
  st_mig_county_to_county <- rbind(st_mig_county_to_county, elderly_mig)
  
  rm(elderly_mig)
  
}

st_mig_county_to_county <- st_mig_county_to_county[-1, ]

st_mig_county_to_county[ , c("estimate_10000")] <- (1 / 10000) * st_mig_county_to_county[ , c("estimate")]


```

```{r, make Dubois map of elderly migration, eval = FALSE, results = FALSE, echo= FALSE}

# importing jefferies font from project working directory folder

windows <- "C:/Windows/Fonts"

font_import(paths = windows, prompt = FALSE)

########### ask about jefferies font

```


## Dubois-Style Line Graph of Within State, County to County Migration for Different Elderly Age Groups 

```{r, plotting in Dubois style 2, warning=FALSE}


ggplot(st_mig_county_to_county, aes(y = estimate_10000, x = year, group = variable, linetype = variable)) +
  geom_line(size = 0.5) +
  # reverse the y-axis and format breaks
  scale_y_reverse(breaks = c(8:35),
                  expand = c(0, 0)) +
  # format x-axis breaks
  scale_x_continuous(breaks = c(2006:2019),
                     expand = c(0, 0)) +
  # add plot title and labels
  labs(linetype = NULL,
       y = "Estimated Number of People \n (tens of thousands)",
       x = NULL,
       title = "U.S. WITHIN STATE, COUNTY TO COUNTY\nELDERLY MIGRATION\n(2006 to 2019)") +
  # apply theme_dubois()
  theme_dubois() + 
  # flip plot coordinates to match original
  coord_flip()
```




```{r, plot in Dubois style, doing one line, eval = FALSE, echo = FALSE}

## Replicate the line graph using theme_dubois()

ggplot(st_mig_county_to_county, aes(x = year)) +
  geom_line(aes(y = est_elder_mig), linetype = "solid",  size = 0.5) + 
  # reverse the y-axis and format breaks
  scale_y_reverse(breaks = seq(400000, 800000, 50000),
                  expand = c(0, 0)) +
  # format x-axis breaks
  scale_x_continuous(breaks = c(2006:2019),
                     expand = c(0, 0)) +
  # add plot title and labels
  labs(linetype = NULL,
       y = "Estimated Total",
       x = NULL,
       title = "Total Migration county to county with in state for Elderly and Non-elderly \n (2006 to 2019)") +
  # apply theme_dubois()
  theme_dubois() + 
  # flip plot coordinates to match original
  coord_flip()

```




<img src="images/dummy_thumbnail.jpg?raw=true"/>
