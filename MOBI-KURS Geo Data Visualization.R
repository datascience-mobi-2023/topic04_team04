# ////////////////////////////////////////////////////////////////////////////////
#    Geo Data Visualization Intro for Molecular Biotechnology (4th Semester)
# ////////////////////////////////////////////////////////////////////////////////


# ////////////////////////////////////////////////////
# # Example 01: Map with coordinate points (centroids)

# load point data
centroids <- read.table(file = "../data/own_data/centroids_df_19042023.csv", 
                        sep = ",", header = TRUE)
#   Districts  Districts_names Longitude Latitude
#           1           ACHHAM     81.30    29.11
#   ...

# load shape file for the map 
# proposed datasource: https://gadm.org/download_country_v3.html
# install.packages("sf")
library(sf)
geography <- st_read("../data/nepal_shape_files/geoBoundaries-NPL-ADM2geocodes/geoBoundaries-NPL-ADM2geocodes.shp")
#        DISTRICT LEVEL                    shapeID shapeGroup shapeType                       geometry
#          ACHHAM   707 NPL-ADM2-48590121B54588117        NPL      ADM2 POLYGON ((81.46795 29.27183...

# join the two data sources
# make sure to have a matching column (district numbers would be better)
centroids$Districts_names[centroids$Districts_names == "KABHREPALANCHOK"] <- "KAVREPALANCHOK" 
centroids$Districts_names[centroids$Districts_names == "MAKAWANPUR"] <- "MAKWANPUR"
centroids$Districts_names[centroids$Districts_names == "NAWALPARASI_E"] <- "NAWALPARASI EAST"
centroids$Districts_names[centroids$Districts_names == "NAWALPARASI_W"] <- "NAWALPARASI WEST"
centroids$Districts_names[centroids$Districts_names == "RUKUM_E"] <- "RUKUM EAST"
centroids$Districts_names[centroids$Districts_names == "RUKUM_W"] <- "RUKUM WEST"

# Adjust column header
names(geography)[1]  <- "Districts_names"

# join
# install.packages("dplyr")
library(dplyr)
geo_data <- left_join(x = centroids, y = geography, by = join_by(Districts_names))

# get an overview what data object you are dealing with
class(geo_data) 
# [1] "data.frame"
head(geo_data)
#   Districts Districts_names Longitude Latitude LEVEL                    shapeID shapeGroup shapeType
#           1          ACHHAM     81.30    29.11   707 NPL-ADM2-48590121B54588117        NPL      ADM2
#                        geometry
#   POLYGON ((81.46795 29.27183...
summary(geo_data)
str(geo_data)

# create an sf object for plotting
geo_data <- sf::st_as_sf(geo_data)
class(geo_data)
# [1] "sf"         "data.frame"

# plot with basic plot function of R
plot(geo_data) # "all data columns" are plotted

# if plotting does not start par(mar = c(0, 0, 0, 0)) OR dev.off() can sometimes help
par(mar = c(0, 0, 0, 0)) 
plot(geo_data[3]) # single column

# plot point data with ggplot
# install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_sf(data = geo_data, col = "black", fill = NA) +  # adds geometric layer of polygons stored in the data object
  geom_point(data = geo_data, aes(x = Longitude, y = Latitude)) # adds coordinate points



# ///////////////////////////////////////////////////////
# # Example 02: Map with data plotted over district area

# plot polygon data with ggplot
# district data dataframe with geometry column as last column
district_sf <- sf::st_read("../data/own_data/district_geodata_13042023.shp")
class(district_sf) # [1] "sf"         "data.frame"

ggplot() +
  geom_sf(data = district_sf, aes(fill = pop_cnt)) + # state your data source and which column information you want to plot
  coord_sf()

# adjustments: e.g. you can change themes
ggplot() +
  geom_sf(data = district_sf, aes(fill = pop_cnt)) +
  coord_sf() +  
  theme_minimal() # no grey background


# adjustments: color palette
# install.packages("viridis")
library(viridis)
ggplot() +
  geom_sf(data = district_sf, aes(fill = pop_cnt)) +
  coord_sf() +
  scale_fill_viridis()  # Set the color palette to viridis

ggplot() +
  geom_sf(data = district_sf, aes(fill = pop_cnt)) +
  coord_sf() +
  scale_fill_gradient(low = "white", high = "blue")  # Set own color palette


# facet plots are possible
ggplot() +
  geom_sf(data = district_sf) +
  facet_wrap(~pop_cnt) + # splitts shapes into different graphs
  coord_sf() 



# /////////////////////////////////////////////////////////////////////////////
# Example 03: Plot data points with color and size according to e.g. incidence 
# from great resource: https://rpubs.com/Dr_Gurpreet/spatial_data_visualisation_R and https://hughst.github.io/
# !!! Here you can try around with the data, as it is available on the internet)
ETH_malaria_data <- read.csv("https://raw.githubusercontent.com/HughSt/HughSt.github.io/master/course_materials/week1/Lab_files/Data/mal_data_eth_2009_no_dups.csv",
                             header=T)


shp <- sf::st_read("../../../Downloads/ETH_Adm1_shapefile/ETH_Adm1_shapefile/ETH_Adm_1.shp") # you will have to adjust this

dat <- sf::st_as_sf(
  ETH_malaria_data,
  coords = c("longitude", "latitude"),
  crs = 4326 
)
# CRS code 4326 corresponds to the World Geodetic System 1984 (WGS84)
# coordinates are expressed as decimal degrees (long & lat from -180 to 180)

# look for the column you want to plot 
names(dat)

ggplot() +
  geom_sf(data = shp) +  # gets the mapping data for the background
  geom_sf(data = dat, aes(size = pf_pr, color = pf_pr)) + # gets the data information with according coordinate point on the map
  # geom_sf(data = dat, aes(size = pf_pr)) + # you can try around and only use one of the eastetics aes()
  labs(title = "Location of schools studied in Ethopia with size and color according to falciparum rate",
       color = "Falciparum Rate",
       size = "Falciparum\nRate") + # changes agenda labels
  xlab("Longitude") + 
  ylab("Latitude") + # changes axsis lables
  ggspatial::annotation_north_arrow(location = "br") + # adds arrow indicating north for maps
  ggspatial::annotation_scale(location = "bl") # adds scale to maps



# ////////////////////////////////////////////////////////////////////////////////////
# Example 04: Geo facet plot inspired from this publication (Advanced)
# https://www.sciencedirect.com/science/article/pii/S2542519620302928?via%3Dihub#sec1
# https://github.com/drrachellowe/hydromet_dengue
# install.packages(...)
library(RColorBrewer)
library(geofacet)
library(ggplot2)
library(dplyr)

# load geographic district data
geography <- sf::st_read("../data/own_data/district_geodata_13042023.shp")

# create a column grid from your map/shapefile information
grid <- geofacet::grid_auto(geography)
names(grid) <- c("row", "col", "code", "name") 
# grid <- data.frame(code = grid$adm2code, name = grid$dname, row = grid$row, col = grid$col)
# naming of the columns of the grid object can be tricky 
# it makes a difference for accessing the right information for the ggplot later
# write.csv(grid, "../data/own_data/grid_10052023.csv", row.names=FALSE)


# load case data 
data <- read.table(file = "../data/own_data/data_15042023.csv", sep = "," , header = TRUE)

# that is how my case data with additional district information looked 
#          date year month day adm2code  adm2name cases    sex  age     pop density   ageprop5 growth piped_water
#    2019-06-10 2019     6  10      510      DANG     1   Male 34.0  657455     228 0.08726833   1.92    56.14115
#   d_long d_lat   t2m_av  t2m_max
#    82.40 27.97 30.52467 35.71827

# plot the geofacet plot
cases_facet <- 
  data %>%  # this connects the data object
  group_by(year, month, day, adm2code) %>% # this groups the cases per month and year for each district
  # calculate state level incidence rate
  summarise(cases = sum(cases)) %>% 
  ggplot(aes(x = month, y = year, fill = cases)) + # for each facet plot it sets the time which is summarized per x and y axis
  geom_raster() + # creates a raster-based heatmap representing the data in color spectrum
  ylab("Year") + 
  xlab("Month") +
  scale_fill_gradientn(name = "# Dengue Cases", colours=brewer.pal(7, "PuRd"), trans = "log1p", breaks = c(0, 10, 100, 300, 1000), labels = c(0, 10, 100, 300, 1000) ) + 
  scale_y_discrete() +
  scale_x_discrete(labels = c("", "2", "", "4", "", "6", "", "8", "", "10", "", "12")) + # improved readability of the axis
  theme_bw() + 
  theme(plot.title = element_text(size = 25), axis.title = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  # organise by district code in grid file
  facet_geo( ~ adm2code, grid = grid, label = "code") # arranges the facet plots position to each other

cases_facet

# ggsave("../figures/geofacet_cases.eps", height = 30, width = 50, units = "cm")



# ///////////////////////////////////////////////////////////////////////////////////////////////////////
# # Example 05: Raster data reading in, plotting, cropping to area of interest (Advanced)
# # & summarizing data using shapefile polygons (you already got this information before)

# HFI stands for Human Footprint Index in this example

# load libraries (order can matter!!!)
library(sp)
library(sf)
library(terra)
library(raster)
library(ggplot2)
library(exactextractr)

# Load raster into R
hfi <- raster("../data/hfp_Index_2018/hfp2018.tif")

# View raster structure
hfi
# class      : RasterLayer 
# dimensions : 16382, 36081, 591078942  (nrow, ncol, ncell)
# resolution : 1000, 1000  (x, y)
# extent     : -18040094, 18040906, -7363043, 9018957  (xmin, xmax, ymin, ymax)
# crs        : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
# source     : hfp2018.tif 
# names      : hfp2018 
class(hfi) 
# [1] "RasterLayer"

# plot
par("mar")
par(mar=c(1,1,1,1))
plot(hfi)

# Cut out region around Nepal
# load shapefile 
geography <- st_read("../data/nepal_shape_files/geoBoundaries-NPL-ADM2geocodes/geoBoundaries-NPL-ADM2geocodes.shp")[,c(2,6)]
class(geography)
# [1] "sf"         "data.frame"

# convert shapefile into 'SpatialPolygonsDataFrame'
geography <- as_Spatial(geography)
class(geography)

library(rgdal) # will expire
geography_proj <- spTransform(geography, CRS(proj4string(hfi)))
# [1] "SpatialPolygonsDataFrame"

# Cropping raster according to the geographical extent of the map information in "geography"
hfi_nepal <- crop(hfi, extent(geography_proj), mask = TRUE, small = TRUE)
hfi_nepal # new raster obtained
# class      : RasterLayer 
# dimensions : 485, 883, 428255  (nrow, ncol, ncell)
# resolution : 1000, 1000  (x, y)
# extent     : 7362906, 8245906, 3213957, 3698957  (xmin, xmax, ymin, ymax)
# crs        : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
# source     : memory
# names      : hfp2018 
# values     : 0.0001156851, 50  (min, max)

# plot
par(mar=c(1,1,1,1))
plot(hfi_nepal)

# Calculate the means of HFI values for all districts --> gives a vector of one values per district
hfi_district_mean <- exact_extract(hfi_nepal, as(geography, "SpatialPolygonsDataFrame"), 'mean') # you can also use other calculations here

# creating a dataframe integrating this new HFI information with district name/code and geometry information
hfi_df <- data.frame(adm2code = geography$LEVEL, hfi = hfi_district_mean, geometry = sf::st_as_sf(geography)[,2])

# Plot new hfi object
library(ggplot2)
library(sf)

# Convert the hfi data.frame to an sf object
hfi_sf <- st_as_sf(hfi_df, coords = c("longitude", "latitude"), crs = 4326)

# Plot the hfi variable with a color gradient

# library(ggthemes)
ggplot(hfi_df) + 
  geom_sf(aes(fill = hfi, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "red") +
  # scale_fill_gradient_tableau("Blue-Teal") +
  labs(fill = "HFI") + 
  theme_void()

# Save
# write.csv(hfi_df[,c(1,2)], "../data/own_data/hfi_geomdf_19042023.csv", row.names=FALSE)



geography <- st_read("../../MoBi_Kurs/shapefiles/gadm36_THA_shp/gadm36_THA_1.shp")
length(geography)
dim(geography)
