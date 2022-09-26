# Prepare Environment -----------------------------------------------------

# install.packages("devtools")
library(tidyverse)
library(leaflet)
library(readxl)

install.packages("rgeos")
library(rgeos)

install.packages("rgdal")
library(rgdal)

install.packages("maptools")
library(maptools)

library(sp)
library(readxl)
library(openxlsx)


rm(list=ls())

# Load Deprivation Data ---------------------------------------------------
NW_LTLA<-read_excel("NW_LTLA_Mapping.xlsx")

imd<-read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = 2)

imd<-imd%>%
  left_join(NW_LTLA, c("Local.Authority.District.code.(2019)" = "areaCode"))%>%
  mutate(`Index.of.Multiple.Deprivation.(IMD).Decile` = factor(`Index.of.Multiple.Deprivation.(IMD).Decile`))


# Read ShapeFile ----------------------------------------------------------

lsoa<- geojsonio::geojson_read(
  x = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

# Join Shape Files with Data ----------------------------------------------

lsoa_data<-merge(lsoa, imd, by.x = "LSOA11CD", "LSOA.code.(2011)")

##Filter for North West
lsoa_data_nw<-subset(lsoa_data, Region == "North West")

# Create Palettes --------------------------------------------------------

pal_fac<-colorFactor(palette = "Blues", levels = 10:1, domain = lsoa_data_nw$`Index.of.Multiple.Deprivation.(IMD).Decile`)

# Create map --------------------------------------------------------------

lat<-53.8
lng<--2.5

leaflet(lsoa_data_nw)%>%
  addPolygons(stroke = TRUE, weight = 0.4, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_fac(`Index.of.Multiple.Deprivation.(IMD).Decile`),
              label = paste0(lsoa_data_nw$LSOA11NM,": Decile - ", lsoa_data_nw$`Index.of.Multiple.Deprivation.(IMD).Decile`))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = lsoa_data_nw$`Index.of.Multiple.Deprivation.(IMD).Decile`, pal = pal_fac, title = "IMD Decile - 2019",position = "topright")%>%
  setView(lng, lat, zoom = 8)


# Create C&M Map ----------------------------------------------------------

lat<-53.3
lng<--2.5

lsoa_data_cm<-subset(lsoa_data_nw, System == "Cheshire & Merseyside")

leaflet(lsoa_data_cm)%>%
  addPolygons(stroke = TRUE, weight = 0.4, opacity = 1, fillOpacity = 0.6,
              color = "black", fillColor = ~pal_fac(`Index.of.Multiple.Deprivation.(IMD).Decile`),
              label = paste0(lsoa_data_cm$LSOA11NM,": Decile - ", lsoa_data_cm$`Index.of.Multiple.Deprivation.(IMD).Decile`))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = lsoa_data_cm$`Index.of.Multiple.Deprivation.(IMD).Decile`, pal = pal_fac, title = "IMD Decile - 2019",position = "topright")%>%
  setView(lng, lat, zoom = 9)



# Create C&M Map with LA outline ------------------------------------------



lads <- geojsonio::geojson_read(
  x = "regions.geojson",  # the same file_path as above
  what = "sp"  # returns the read with spatial class
)

##Filter shapes for North West only
NW_LTLA<-read_excel("NW_LTLA_Mapping.xlsx")
lads_nw<-merge(lads,NW_LTLA, by.x = "lad17cd", "areaCode")
lads_nw<-subset(lads_nw, Region == "North West")
lads_cm<-subset(lads_nw, System == "Cheshire & Merseyside")

leaflet(lsoa_data_cm)%>%
  addPolygons(data = lads_cm, color = "black", weight = 2)%>%
  addPolygons(stroke = TRUE, weight = 0.4, opacity = 1, fillOpacity = 0.6,
              color = "grey", fillColor = ~pal_fac(`Index.of.Multiple.Deprivation.(IMD).Decile`),
              label = paste0(lsoa_data_cm$LSOA11NM,": Decile - ", lsoa_data_cm$`Index.of.Multiple.Deprivation.(IMD).Decile`))%>%
  addProviderTiles(providers$CartoDB.Voyager)%>%
  addLegend(values = lsoa_data_cm$`Index.of.Multiple.Deprivation.(IMD).Decile`, pal = pal_fac, title = "IMD Decile - 2019",position = "topright")%>%
  setView(lng, lat, zoom = 9)

