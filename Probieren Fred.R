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

library(ggplot2)
ggplot() +
  geom_sf(data = geo_data, col = "black", fill = NA) +  # adds geometric layer of polygons stored in the data object
  geom_point(data = geo_data, aes(x = Longitude, y = Latitude))



ggplot+
  geom_sf(data = district_sf$geometry,) +   # state your data source and which column information you want to plot
  labs(title = "Thailand")
  
 












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


















  