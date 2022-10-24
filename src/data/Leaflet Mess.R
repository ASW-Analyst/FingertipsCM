
# Prepare Environment -----------------------------------------------------

# install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(tidyverse)
library(leaflet)

install.packages("rgeos")
library(rgeos)

install.packages("rgdal")
library(rgdal)

install.packages("maptools")
library(maptools)

library(sp)

rm(list=ls())


# Determine Indicator id and Load Fingertips data -------------------------

df <- profiles(ProfileName = "Wider Determinants of Health")
print(df)

list <- indicators()
indicator_id<-93378

master_data<- fingertips_data(IndicatorID = indicator_id, AreaTypeID = 401)

map_ind_data<-master_data%>%
  filter(ParentName == "North West region",
         Timeperiod == '2020/21')


# Download ShapeFile ------------------------------------------------------

lads <- geojsonio::geojson_read(
  x = "geo/region.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)



# Join Shape Files with Data ----------------------------------------------

lads_data<-merge(lads, map_ind_data, by.x = "lad17cd", "AreaCode")


# Create Palettes --------------------------------------------------------



bins <- c(30,40,50,60)
pal_bin<-colorBin("Greens", lads_data$Value, bins = 4)
pal_con<-colorNumeric("Greens", lads_data$Value)


# Create map --------------------------------------------------------------

lat<-53.8
lng<--2.5

leaflet(lads_data)%>%
  addPolygons(stroke = TRUE, weight = 1, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_bin(Value),
              label = paste0(lads_data$lad17nm,": ", lads_data$Value," - (", round(lads_data$LowerCI95.0limit,1)," - ",round(lads_data$UpperCI95.0limit,1), ")"))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = lads_data$Value, pal = pal_bin, title = "Attainment Score",position = "topright")%>%
  setView(lng, lat, zoom = 8)
  

