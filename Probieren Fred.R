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
          
            

  