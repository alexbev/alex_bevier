library(sf)
library(dplyr)
library(tmap)


# setup
nhoods <- st_read('Neighborhoods.shp')
zip <- st_read('zip_codes.shp')
crime <- st_read('RMS_Crime_Incidents.shp')
grocery <- st_read('Grocery_Stores_in_City_of_Detroit_Public_View.shp')

nhoods_p <- st_transform(nhoods, crs = 5623)
zip_p <- st_transform(zip, crs = 5623)
crime_p <- st_transform(crime, crs = 5623)
grocery_p <- st_transform(grocery, crs = 5623)


crime21 <- crime_p %>% 
  filter(year == 2021)
st_is_longlat(crime21)

# Task 1 Question 1

# set up tmaps
tmap_mode('plot')
tmap_mode('view')

colnames(crime21)
colnames(nhoods_p)

# create a matrix of all crimes for each neighborhood, apply it to count for each neighborhood, then bind the data with the neighborhood data.
combined_crime_data <- st_within(crime21, nhoods_p, sparse = FALSE)
cbn <- apply(X = combined_crime_data, MARGIN = 2, FUN = sum)
comb_data <- cbind(nhoods_p, cbn)

# filter the combined data for the given parameters
low_crime <- comb_data %>% 
  filter(cbn < 100)
high_crime <- comb_data %>% 
  filter(cbn > 1200)

# create the neighborhood map with low and high crime data
tm_shape(nhoods_p) + tm_polygons(alpha = 0.4) +
  tm_shape(low_crime) + tm_polygons(col = 'green', alpha = 0.5) +
  tm_shape(high_crime) + tm_polygons(col = 'red', alpha = 0.5) +
  tm_layout(main.title = 'Map of Detroit Neighborhoods with Safety Ratings',
            main.title.position = 'center',
            main.title.size = 1,
            frame = FALSE
  )

# Task 1 Question 2
# same process as above, except by zip code and not by neighborhood

zip_data <- st_within(crime21, zip_p, sparse = FALSE)
cbz <- apply(X = zip_data, MARGIN = 2, FUN = sum)
zdata <- cbind(zip_p, cbz)

low_crime_z <- zdata %>% 
  filter(cbz < 1000)
high_crime_z <- zdata %>% 
  filter(cbz > 5000)

tm_shape(zip_p) + tm_polygons(alpha = 0.3) + tm_text('zipcode', size = 0.5) +
  tm_shape(low_crime_z) + tm_polygons(col = 'green', alpha = 0.5) +
  tm_shape(high_crime_z) + tm_polygons(col ='red', alpha = 0.5) +
  tm_layout(main.title = 'Map of Detroit Zip Codes with Safety Ratings',
            main.title.position = 'center',
            main.title.size = 1,
            frame = FALSE
  )

# Task 2 Question 1

# create half mile and mile buffers
gbuff_half <- st_buffer(grocery_p, dist = 2640)
gbuff_whole <- st_buffer(grocery_p, dist = 5280)

# create map with the neighborhood data, buffer data, and grocery store data
tm_shape(nhoods_p) + tm_polygons(alpha = 0.3) +
  tm_shape(gbuff_half) + tm_borders(col = 'blue', alpha = 0.3) + tm_fill(col = 'blue', alpha = 0.3) +
  tm_shape(gbuff_whole) + tm_borders(col = 'red', alpha = 0.3) + tm_fill(col = 'red', alpha = 0.3) +
  tm_shape(grocery_p) + tm_dots(col = 'yellow', alpha = 1, size = 0.1) +
  tm_layout(main.title = 'Map of Detroit Grocery Store with 1/2 Mile and Mile Radii',
            main.title.position = 'center',
            main.title.size = 1,
            frame = FALSE
  )
  
# Task 2 Question 2

# filter all 2021 crime data for just robbery data
robberies <- crime21 %>% 
  filter(offense_de == 'ROBBERY')

# create 2 matrices for the number of robberies within a half mile and mile of each grocery store
rob_in_half <- st_within(robberies, gbuff_half, sparse = FALSE)
rob_in_whole <- st_within(robberies, gbuff_whole, sparse = FALSE)

# apply matrices to tally for each grocery store
robs_half <- apply(X = rob_in_half, MARGIN = 2, FUN = sum)
robs_whole <- apply(X = rob_in_whole, MARGIN = 2, FUN = sum)


# bind the grocery data, 1/2 mile buffer data AND full mile buffer data together
grocery_robs <- cbind(grocery_p, robs_half, robs_whole)

# descriptive statistics for robberies within 1/2 mile and within mile
summary(robs_half)
summary(robs_whole)

# take the completely binded data and filter for the given columns, drop geometery
groc_rob_data_export <- grocery_robs %>% 
  select(Store_Name, Address, robs_half, robs_whole)
groc_rob_data_export <- st_as_sf(groc_rob_data_export) %>% 
  st_drop_geometry()

# export as an excel file
library(writexl)
write_xlsx(groc_rob_data_export, 'detroit grocery stores and robberies.xlsx')
