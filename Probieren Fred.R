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




district_sf <- st_read("/Users/frederik/Documents/GitHub/Projekt/gadm36_THA_shp/gadm36_THA_1.shp")
class(district_sf) # [1] "sf"         "data.frame"

plot(district_sf)


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
ggplot(data = data_total_test, mapping= aes(data_total_test$time_column))














  