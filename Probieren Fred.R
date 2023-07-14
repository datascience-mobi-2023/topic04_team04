library(dplyr)
library(readxl)

Endung <- "_dengue_extracted.xlsx"
Anfang <- "Dengue_ohne_Jahr"

for (i in 2006:2020) {
  Name <- paste0(i, Endung) #erstellen das Dateinamens
  Name_df <- paste0(Anfang, i) 
  assign(Name_df, read_excel(Name)) #erstellen der Variable und zuweisen der Excel-Tabelle 
  as.data.frame(Name_df) #umwandeln der Tabelle in ein Data-Frame
  m = get(Name_df)
  
  
  
  for(j in 1:77){
  m$Data[j,] = paste0(m[j,1], i)
  }
  
  
}


Vektor <- c(Dengue2006[,1])

Vektor
          
#Mapping von Thailand

library(sf)
library(ggplot2)
library(dplyr)

Mapping_dengue_2006Apr <- rows_2006Apr


district_sf <- st_read("/Users/frederik/Documents/GitHub/Projekt/gadm36_THA_shp/gadm36_THA_1.shp")
class(district_sf) # [1] "sf"         "data.frame"

names(district_sf)[4] <-"Reporting_areas"


geo_data <- left_join(Mapping_dengue_2006Apr, district_sf, by= join_by(Reporting_areas))

class(geo_data)
head(geo_data)
geo_data <- sf::st_as_sf(geo_data)
class(geo_data)
plot(geo_data)



# plottet die Karte von allen distrikten
ggplot() +
  geom_sf(data = geo_data, col = "black", fill = NA) +  # adds geometric layer of polygons stored in the data object
  geom_point(data = geo_data, aes(x = Longitude, y = Latitude))

district_sf <- st_read("/Users/frederik/Documents/GitHub/Projekt/gadm36_THA_shp/gadm36_THA_1.shp")

Inzidenz2006Apr <- rows_2006Apr$incidence

district_sf$incidence<- rows_2006Apr$incidence


ggplot() +
  geom_sf(data = district_sf, aes(fill = incidence)) +
  scale_fill_gradient(low = "blue", high = "red")


ggplot()+
  geom_sf(data = district_sf, aes(fill= AREA))
  

ggplot() +
  geom_sf(data = district_sf) + # state your data source and which column information you want to plot
  coord_sf()
 












#calculating incidence

dimensions= dim(data_total)
R= dimensions[1]

data_total_test <- data_total

for(Zeile in 1:R){
  
  Population <- data_total_test$population_count[Zeile]
  Infection <- data_total_test$dengue_cases[Zeile]
  Incidence = (Infection/Population)*100000
  data_total_test$incidence[Zeile] <- Incidence
}


#plotting incidence


plot_inzidenz <- c()

district <- "Amnat Charoen"  
plot_zeit <- c()



zeit_nehmen <- function(data, district, plot_zeit){
  if(data$Reporting_areas==district)
  plot_zeit <- plot_zeit + data$time_column[]
}

while (data_total_test$Reporting_areas=="Amnat Charoen") {
  i
  
}


result <- apply(data_total_test, district, plot_zeit)


plot_zeit <- data_total_test$time_column[data_total_test$Reporting_areas=="Amnat Charoen"| data_total_test$year==2008]
plot_inzidenz <- data_total_test$Incidence[data_total_test$Reporting_areas=="Amnat Charoen" || data_total_test$year==2008]


plot(plot_zeit, plot_inzidenz)




#Fusionieren der distrikte

data_total_test <- data_total

for (i in 1:dim(data_total)[1]) {
  
  if(data_total$Reporting_areas[i]=="Bungkan"){
    Datum <- data_total$time_column[i]
    
    for(j in 1:dim(data_total)[1]){
      if(data_total$Reporting_areas[j] == "Nong Khai" & data_total$time_column[j] == Datum){
        
        data_total$dengue_cases[j]<- ifelse(is.na(data_total$dengue_cases[i]), data_total$dengue_cases[j], data_total$dengue_cases[i]+data_total$dengue_cases[j])
        data_total$population_count[j]<- ifelse(is.na(data_total$population_count[i]), data_total$population_count[j], data_total$population_count[i]+data_total$population_count[j])
        data_total$incidence[j]<- ifelse(is.na(data_total$incidence[i]), data_total$incidence[j], data_total$incidence[i]+data_total$incidence[j])
        
      }
    }
  }
}
data_total_test <- data_total_test[data_total_test$Reporting_areas != "Bungkan", ]










dengue_spatial_plotting = 

shp <- sf::st_read("/Users/frederik/Documents/GitHub/Projekt/gadm36_THA_shp/gadm36_THA_1.shp") # you will have to adjust this



dat <- sf::st_as_sf(
  ETH_malaria_data,
  coords = c("longitude", "latitude"),
  crs = 4326 
)
# CRS code 4326 corresponds to the World Geodetic System 1984 (WGS84)
# coordinates are expressed as decimal degrees (long & lat from -180 to 180)
​
# look for the column you want to plot 
names(dat)
​
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
​
​














  