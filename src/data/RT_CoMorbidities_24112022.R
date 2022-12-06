# Co-Morbidities at CCG Level in the North West
# Atrial fibrillation

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

atrial_fib<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                sheet = 2, startRow = 12)
atrial_fib<-atrial_fib%>%
  select(1:3, 9:11)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                               "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                               "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                               "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                               "E38000226","E38000170","E38000187")) 
AF<-rename(atrial_fib, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, AF, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Atrial Fibrillation (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Coronary heart disease

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

coronary_heart<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                      sheet = 4, startRow = 12)
coronary_heart<-coronary_heart%>%
  select(1:3, 9:14)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
CHD<-rename(coronary_heart, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, CHD, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Coronary Heart Disease (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Heart failure

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

heart_fail<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                          sheet = 5, startRow = 12)
heart_fail<-heart_fail%>%
  select(1:3, 9:11)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
HF<-rename(heart_fail, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, HF, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Heart Failure (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Hypertension

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

hypertension<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                      sheet = 6, startRow = 12)
hypertension<-hypertension%>%
  select(1:3, 9:14)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
HYP<-rename(hypertension, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, HYP, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Hypertension (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Peripheral Arterial Disease 

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

peripheral_art<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                        sheet = 7, startRow = 12)
peripheral_art<-peripheral_art%>%
  select(1:3, 9:11)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
PAD<-rename(peripheral_art, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, PAD, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Peripheral Arterial Disease (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Stroke and Transient Ischaemic Attack 

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

stroke_TIA<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                          sheet = 8, startRow = 12)
stroke_TIA<-stroke_TIA%>%
  select(1:3, 9:14)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
STIA<-rename(stroke_TIA, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, STIA, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Stroke and Transient Ischaemic Attack (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Obesity 

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

obesity<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                      sheet = 11, startRow = 12)
obesity<-obesity%>%
  select(1:3, 9:11)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
OB<-rename(obesity, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, OB, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Stroke and Transient Ischaemic Attack (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Co-Morbidities at CCG Level in the North West
# Depression

#install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)
library(readxl)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(openxlsx)

rm(list=ls())

## CCG Shape File

ccg<-geojsonio::geojson_read(
  x = "geo/Clinical_Commissioning_Groups_(April_2021)_EN_BSC.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

## Filter shapes for North West only
NW_CCG<-read_excel("ref/NW_CCG.xlsx")
ccg_nw<-merge(ccg, NW_CCG, by.x = "CCG21CD", "Area Code")
ccg_cm<-subset(ccg_nw, System == "Cheshire & Merseyside")

## Load Co-Morbidity Data

depression<-read.xlsx("https://files.digital.nhs.uk/D9/13B97D/qof-2122-sicbl-ach-prev-pca.xlsx", 
                   sheet = 19, startRow = 12)
depression<-depression%>%
  select(1:3, 9:11)%>%
  filter(Sub.ICB.Loc.ONS.code %in% c("E38000015","E38000217","E38000091","E38000143","E38000014","E38000101",
                                     "E38000080","E38000068","E38000050","E38000205","E38000135","E38000227",
                                     "E38000024","E38000172","E38000182","E38000161","E38000194","E38000174",
                                     "E38000200","E38000034","E38000016","E38000208","E38000228","E38000233",
                                     "E38000226","E38000170","E38000187")) 
DEP<-rename(depression, Prevalence = "Prevalence.(%)")

## Joining Shape Data and QOF Data
ccg_data_cm<-merge(ccg_cm, DEP, by.x = "CCG21CD",  "Sub.ICB.Loc.ONS.code")


## Creating Palettes

pal_con<-colorNumeric("Greens", ccg_data_cm$`Prevalence`)

lat<-53.3
lng<--2.5

leaflet(ccg_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_con(Prevalence),
              label = paste0(ccg_data_cm$CCG21NM,": ", round(ccg_data_cm$Prevalence, 2)))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = ccg_data_cm$Prevalence, pal = pal_con, title = "Prevalence of Depression (%)",position = "topright")%>%
  setView(lng, lat, zoom = 9)
  