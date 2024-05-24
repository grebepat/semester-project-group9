# Import the messenger data

#Load Packages

library("dplyr")
library("ggplot2")
library("tidyr")
library("sf")
library("terra")
library("tmap")
library("zoo")
library("zoo")
library("units")
library("plotly")
library("patchwork")
library("tidyverse")
library("rjson")
library("jsonlite")
library("leaflet")
library("XML")
library("lubridate")


#Task 1: Import your data

# generate a list of all filenames including the path from the subfolder they are stored in
file_list <- list.files("Strava Data Chahan/activities/", recursive = TRUE, pattern = "\\.gpx$", full.names = TRUE)


# Get Path of the subfolders to extraxt names of subfolders and / or filenames
# Credits go to: https://stackoverflow.com/questions/54082781/extract-portion-of-file-name-using-gsub
file_name <- gsub('.*/(.*).gpx','\\1', file_list)

# creating empty vessels for our soon to be imported data using a for loop
single_routes <- list()
all_routes <- data.frame()


# create a loop to read in every single gpx-file in the subfolder and extract the needed values such as coordinates, elevation and timestamps
for (file in file_list) {
  # Parse the GPX file using the htmlTreeParse() Function
  # Credits go to: https://www.appsilon.com/post/r-gpx-files
  gpx_parsed <- htmlTreeParse(file = file, useInternalNodes = TRUE)
  
  # Extract data from the parsed GPX file
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
  time <- xpathSApply(doc = gpx_parsed, path = "//time", fun = xmlValue)
  name <- xpathSApply(doc = gpx_parsed, path = "//name", fun = xmlValue)
  
  # Get Path of the subfolders to extract names of subfolders
  
  
  # Generate new data frame with extracted values
  df <- data.frame(
    time = time[-1],
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation),
    
    # Extract filenames and subfoldernames using gsub() credits go to: https://stackoverflow.com/questions/54082781/extract-portion-of-file-name-using-gsub
    id = gsub('.*/(.*).gpx','\\1', file)
    )
  
  # transform into spatial data frame and project to new coordinate system
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  df_sf <- st_transform(df_sf, crs = 2056)
  df_sf$x <- st_coordinates(df_sf)[,1]
  df_sf$y <- st_coordinates(df_sf)[,2]
  df_sf <- select(df_sf, id, time, x, y, elevation, geometry)
  
  
# Store every iteration into a single data frame and send it to the list single_routes()
  single_routes[[file]] <- df_sf
  
# Store every iteration into new rows of the existing data frame all_routes()
  all_routes <- rbind(all_routes, df_sf)
}

# bind all_routes with activity name
names <- read_delim("Strava Data Chahan/activities.csv") %>% 
  select(`AktivitÃ¤ts-ID`, `Name der AktivitÃ¤t`, `AktivitÃ¤tsart`, Dateiname) %>% 
  rename("id" = `AktivitÃ¤ts-ID`, "Name" = `Name der AktivitÃ¤t`, "Aktivität" = `AktivitÃ¤tsart`)

names <- names %>% 
  filter(Aktivität == "Radfahrt" & grepl("\\.gpx$", Dateiname))

# change id in all_routes to numeric
all_routes$id <- as.numeric(all_routes$id)

# join the data
all_routes <- all_routes %>% 
  left_join(names)

# save a example route as object
example_route <- single_routes[["Strava Data Chahan/activities/1836915246.gpx"]]
# download a example single route for exercise 4
write_csv(example_route, "example_route.csv")




